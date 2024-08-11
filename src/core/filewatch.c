/*
* Copyright (c) 2024 Calvin Rose
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to
* deal in the Software without restriction, including without limitation the
* rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
* sell copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
* IN THE SOFTWARE.
*/

#ifndef JANET_AMALG
#include "features.h"
#include <janet.h>
#include "util.h"
#endif

#ifdef JANET_EV

typedef struct {
    JanetTable *watch_descriptors;
    JanetStream *stream;
    JanetChannel *channel;
    uint32_t default_flags;
} JanetWatcher;

#ifdef JANET_LINUX
#include <sys/inotify.h>
#include <unistd.h>
#endif

typedef struct {
    const char *name;
    uint32_t flag;
} JanetWatchFlagName;

static const JanetWatchFlagName watcher_flags_linux[] = {
    {"access", IN_ACCESS},
    {"all", IN_ALL_EVENTS},
    {"attrib", IN_ATTRIB},
    {"close-nowrite", IN_CLOSE_NOWRITE},
    {"close-write", IN_CLOSE_WRITE},
    {"create", IN_CREATE},
    {"delete", IN_DELETE},
    {"delete-self", IN_DELETE_SELF},
    {"ignored", IN_OPEN},
    {"modify", IN_MODIFY},
    {"move-self", IN_MOVE_SELF},
    {"moved-from", IN_MOVED_FROM},
    {"moved-to", IN_MOVED_TO},
    {"open", IN_OPEN},
    {"q-overflow", IN_Q_OVERFLOW},
    {"unmount", IN_UNMOUNT},
};
static uint32_t decode_inotify_flags(const Janet *options, int32_t n) {
    uint32_t flags = 0;
    for (int32_t i = 0; i < n; i++) {
        if (!(janet_checktype(options[i], JANET_KEYWORD))) {
            janet_panicf("expected keyword, got %v", options[i]);
        }
        JanetKeyword keyw = janet_unwrap_keyword(options[i]);
        const JanetWatchFlagName *result = janet_strbinsearch(watcher_flags_linux,
                sizeof(watcher_flags_linux) / sizeof(JanetWatchFlagName),
                sizeof(JanetWatchFlagName),
                keyw);
        if (!result) {
            janet_panicf("unknown inotify flag %v", options[i]);
        }
        flags |= result->flag;
    }
    return flags;
}

static void janet_watcher_init(JanetWatcher *watcher, JanetChannel *channel, uint32_t default_flags) {
    int fd;
    do {
        fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    } while (fd == -1 && errno == EINTR);
    watcher->watch_descriptors = janet_table(0);
    watcher->channel = channel;
    watcher->default_flags = default_flags;
    watcher->stream = janet_stream(fd, JANET_STREAM_READABLE, NULL);
}

static void janet_watcher_add(JanetWatcher *watcher, const char *path, uint32_t flags) {
    if (watcher->stream == NULL) janet_panic("watcher closed");
    int result;
    do {
        result = inotify_add_watch(watcher->stream->handle, path, flags);
    } while (result == -1 && errno == EINTR);
    Janet name = janet_cstringv(path);
    Janet wd = janet_wrap_integer(result);
    janet_table_put(watcher->watch_descriptors, name, wd);
    janet_table_put(watcher->watch_descriptors, wd, name);
}

static void janet_watcher_remove(JanetWatcher *watcher, const char *path) {
    if (watcher->stream == NULL) janet_panic("watcher closed");
    Janet check = janet_table_get(watcher->watch_descriptors, janet_cstringv(path));
    janet_assert(janet_checktype(check, JANET_NUMBER), "bad watch descriptor");
    int watch_handle = janet_unwrap_integer(check);
    int result;
    do {
        result = inotify_rm_watch(watcher->stream->handle, watch_handle);
    } while (result != -1 && errno == EINTR);
    if (result == -1) {
        janet_panicf("%s", janet_strerror(errno));
    }
}

static void watcher_callback_read(JanetFiber *fiber, JanetAsyncEvent event) {
    JanetStream *stream = fiber->ev_stream;
    JanetWatcher *watcher = (JanetWatcher *) fiber->ev_state;
    char buf[1024];
    switch (event) {
        default:
            break;
        case JANET_ASYNC_EVENT_MARK:
            janet_mark(janet_wrap_abstract(watcher));
            break;
        case JANET_ASYNC_EVENT_CLOSE:
            janet_schedule(fiber, janet_wrap_nil());
            fiber->ev_state = NULL;
            janet_async_end(fiber);
            break;
        case JANET_ASYNC_EVENT_ERR:
            {
                janet_schedule(fiber, janet_wrap_nil());
                fiber->ev_state = NULL;
                janet_async_end(fiber);
                break;
            }
    read_more:
    case JANET_ASYNC_EVENT_HUP:
    case JANET_ASYNC_EVENT_INIT:
    case JANET_ASYNC_EVENT_READ:
            {
                Janet name = janet_wrap_nil();

                /* Assumption - read will never return partial events *
                 * From documentation:
                 *
                 * The behavior when the buffer given to read(2) is too small to
                 * return information about the next event depends on the kernel
                 * version: before Linux 2.6.21, read(2) returns 0; since Linux
                 * 2.6.21, read(2) fails with the error EINVAL.  Specifying a buffer
                 * of size
                 *
                 *     sizeof(struct inotify_event) + NAME_MAX + 1
                 *
                 * will be sufficient to read at least one event. */
                ssize_t nread;
                do {
                    nread = read(stream->handle, buf, sizeof(buf));
                } while (nread == -1 && errno == EINTR);

                /* Check for errors - special case errors that can just be waited on to fix */
                if (nread == -1) {
                    if (errno == EAGAIN || errno == EWOULDBLOCK) {
                        break;
                    }
                    janet_cancel(fiber, janet_ev_lasterr());
                    fiber->ev_state = NULL;
                    janet_async_end(fiber);
                    break;
                }
                if (nread < (ssize_t) sizeof(struct inotify_event)) break;

                /* Iterate through all events read from the buffer */
                char *cursor = buf;
                while (cursor < buf + nread) {
                    struct inotify_event inevent;
                    memcpy(&inevent, cursor, sizeof(inevent));
                    cursor += sizeof(inevent);
                    /* Read path of inevent */
                    if (inevent.len) {
                        name = janet_cstringv(cursor);
                        cursor += inevent.len;
                    }

                    /* Got an event */
                    Janet path = janet_table_get(watcher->watch_descriptors, janet_wrap_integer(inevent.wd));
                    JanetKV *event = janet_struct_begin(6);
                    janet_struct_put(event, janet_ckeywordv("wd"), janet_wrap_integer(inevent.wd));
                    janet_struct_put(event, janet_ckeywordv("wd-path"), path);
                    janet_struct_put(event, janet_ckeywordv("mask"), janet_wrap_integer(inevent.mask));
                    janet_struct_put(event, janet_ckeywordv("path"), name);
                    janet_struct_put(event, janet_ckeywordv("cookie"), janet_wrap_integer(inevent.cookie));
                    Janet etype = janet_ckeywordv("type");
                    const JanetWatchFlagName *wfn_end = watcher_flags_linux + sizeof(watcher_flags_linux) / sizeof(watcher_flags_linux[0]);
                    for (const JanetWatchFlagName *wfn = watcher_flags_linux; wfn < wfn_end; wfn++) {
                        if ((inevent.mask & wfn->flag) == wfn->flag) janet_struct_put(event, etype, janet_ckeywordv(wfn->name));
                    }
                    Janet eventv = janet_wrap_struct(janet_struct_end(event));

                    janet_channel_give(watcher->channel, eventv);
                }

                /* Read some more if possible */
                goto read_more;
            }
            break;
    }
}

static void janet_watcher_listen(JanetWatcher *watcher) {
    janet_async_start(watcher->stream, JANET_ASYNC_LISTEN_READ, watcher_callback_read, watcher);
}

/* C Functions */

static int janet_filewatch_mark(void *p, size_t s) {
    JanetWatcher *watcher = (JanetWatcher *) p;
    (void) s;
    janet_mark(janet_wrap_abstract(watcher->stream));
    janet_mark(janet_wrap_abstract(watcher->channel));
    janet_mark(janet_wrap_table(watcher->watch_descriptors));
    return 0;
}

static const JanetAbstractType janet_filewatch_at = {
    "filewatch/watcher",
    NULL,
    janet_filewatch_mark,
    JANET_ATEND_GCMARK
};

JANET_CORE_FN(cfun_filewatch_make,
        "(filewatch/make channel &opt default-flags)",
        "Create a new filewatcher.") {
    janet_arity(argc, 1, -1);
    JanetChannel *channel = janet_getchannel(argv, 0);
    JanetWatcher *watcher = janet_abstract(&janet_filewatch_at, sizeof(JanetWatcher));
    uint32_t default_flags = decode_inotify_flags(argv + 1, argc - 1);
    janet_watcher_init(watcher, channel, default_flags);
    return janet_wrap_abstract(watcher);
}

JANET_CORE_FN(cfun_filewatch_add,
        "(filewatch/add watcher path &opt flags)",
        "Add a path to the watcher.") {
    janet_arity(argc, 2, -1);
    JanetWatcher *watcher = janet_getabstract(argv, 0, &janet_filewatch_at);
    const char *path = janet_getcstring(argv, 1);
    uint32_t flags = watcher->default_flags | decode_inotify_flags(argv + 2, argc - 2);
    janet_watcher_add(watcher, path, flags);
    return argv[0];
}

JANET_CORE_FN(cfun_filewatch_remove,
        "(filewatch/remove watcher path)",
        "Remove a path from the watcher.") {
    janet_fixarity(argc, 2);
    JanetWatcher *watcher = janet_getabstract(argv, 0, &janet_filewatch_at);
    const char *path = janet_getcstring(argv, 1);
    janet_watcher_remove(watcher, path);
    return argv[0];
}

JANET_CORE_FN(cfun_filewatch_listen,
        "(filewatch/listen watcher)",
        "Listen for changes in the watcher.") {
    janet_fixarity(argc, 1);
    JanetWatcher *watcher = janet_getabstract(argv, 0, &janet_filewatch_at);
    janet_watcher_listen(watcher);
    return janet_wrap_nil();
}

/* Module entry point */
void janet_lib_filewatch(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_CORE_REG("filewatch/make", cfun_filewatch_make),
        JANET_CORE_REG("filewatch/add", cfun_filewatch_add),
        JANET_CORE_REG("filewatch/remove", cfun_filewatch_remove),
        JANET_CORE_REG("filewatch/listen", cfun_filewatch_listen),
        JANET_REG_END
    };
    janet_core_cfuns_ext(env, NULL, cfuns);
}

#endif