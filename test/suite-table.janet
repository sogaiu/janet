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

# Denormal tables

(assert (= (length @{1 2 nil 3}) 1) "nil key table literal")
(assert (= (length (table 1 2 nil 3)) 1) "nil key table ctor")

(assert (= (length (table (/ 0 0) 2 1 3)) 1) "nan key table ctor")
(assert (= (length @{1 2 (/ 0 0) 3}) 1) "nan key table literal")

(assert (= (length (table 2 1 3 nil)) 1) "nil value table ctor")
(assert (= (length @{1 2 3 nil}) 1) "nil value table literal")

# Table duplicate elements
(assert (deep= @{:a 3 :b 2} @{:a 1 :b 2 :a 3}) "table literal duplicate keys")
(assert (deep= @{:a 3 :b 2} (table :a 1 :b 2 :a 3))
        "table constructor duplicate keys")

(end-suite)

