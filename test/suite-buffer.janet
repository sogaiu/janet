# Copyright (c) 2023 Calvin Rose
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

(import ./helper :prefix "" :exit true)
(start-suite)

# Buffer blitting

(def b (buffer/new-filled 100))
(buffer/bit-set b 100)
(buffer/bit-clear b 100)
(assert (zero? (sum b)) "buffer bit set and clear")
(buffer/bit-toggle b 101)
(assert (= 32 (sum b)) "buffer bit set and clear")

(def b2 @"hello world")

(buffer/blit b2 "joyto ")
(assert (= (string b2) "joyto world") "buffer/blit 1")

(buffer/blit b2 "joyto" 6)
(assert (= (string b2) "joyto joyto") "buffer/blit 2")

(buffer/blit b2 "abcdefg" 5 6)
(assert (= (string b2) "joytogjoyto") "buffer/blit 3")

# Buffer self blitting, check for use after free
(def buf1 @"1234567890")
(buffer/blit buf1 buf1 -1)
(buffer/blit buf1 buf1 -1)
(buffer/blit buf1 buf1 -1)
(buffer/blit buf1 buf1 -1)
(assert (= (string buf1) (string/repeat "1234567890" 16))
        "buffer blit against self")

# Buffer push word

(def b3 @"")
(buffer/push-word b3 0xFF 0x11)
(assert (= 8 (length b3)) "buffer/push-word 1")
(assert (= "\xFF\0\0\0\x11\0\0\0" (string b3)) "buffer/push-word 2")
(buffer/clear b3)
(buffer/push-word b3 0xFFFFFFFF 0x1100)
(assert (= 8 (length b3)) "buffer/push-word 3")
(assert (= "\xFF\xFF\xFF\xFF\0\x11\0\0" (string b3)) "buffer/push-word 4")

# Buffer push string

(def b4 (buffer/new-filled 10 0))
(buffer/push-string b4 b4)
(assert (= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" (string b4))
        "buffer/push-buffer 1")
(def b5 @"123")
(buffer/push-string b5 "456" @"789")
(assert (= "123456789" (string b5)) "buffer/push-buffer 2")

# Check for bugs with printing self with buffer/format

(def buftemp @"abcd")
(assert (= (string (buffer/format buftemp "---%p---" buftemp))
           `abcd---@"abcd"---`) "buffer/format on self 1")
(def buftemp @"abcd")
(assert (= (string (buffer/format buftemp "---%p %p---" buftemp buftemp))
           `abcd---@"abcd" @"abcd"---`) "buffer/format on self 2")

(end-suite)

