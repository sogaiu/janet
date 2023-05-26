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

(def test-struct {'def 1 'bork 2 'sam 3 'a 'b 'het @[1 2 3 4 5]})
(assert (= (get test-struct 'def) 1) "struct get")
(assert (= (get test-struct 'bork) 2) "struct get")
(assert (= (get test-struct 'sam) 3) "struct get")
(assert (= (get test-struct 'a) 'b) "struct get")
(assert (= :array (type (get test-struct 'het))) "struct get")

# Buffer stuff
(defn buffer=
  [a b]
  (= (string a) (string b)))

(assert (buffer= @"abcd" @"abcd") "buffer equal 1")
(assert (buffer= @"abcd" (buffer "ab" "cd")) "buffer equal 2")
(assert (not= @"" @"") "buffer not equal 1")
(assert (not= @"abcd" @"abcd") "buffer not equal 2")

(defn buffer-factory
  []
  @"im am a buffer")

(assert (not= (buffer-factory) (buffer-factory)) "buffer instantiation")

(assert (= (length @"abcdef") 6) "buffer length")

# Tuple comparison
(assert (< [1 2 3] [2 2 3]) "tuple comparison 1")
(assert (< [1 2 3] [2 2]) "tuple comparison 2")
(assert (< [1 2 3] [2 2 3 4]) "tuple comparison 3")
(assert (< [1 2 3] [1 2 3 4]) "tuple comparison 4")
(assert (< [1 2 3] [1 2 3 -1]) "tuple comparison 5")
(assert (> [1 2 3] [1 2]) "tuple comparison 6")

# issue #928
(assert (= (hash 0) (hash (* -1 0))) "hash -0 same as hash 0")

(end-suite)

