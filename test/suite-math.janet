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

(assert (= 400 (math/sqrt 160000)) "sqrt(160000)=400")

# RNGs

(defn test-rng
  [rng]
  (assert (all identity (seq [i :range [0 1000]]
                             (<= (math/rng-int rng i) i))) "math/rng-int test")
  (assert (all identity (seq [i :range [0 1000]]
    (def x (math/rng-uniform rng))
    (and (>= x 0) (< x 1))))
          "math/rng-uniform test"))

(def seedrng (math/rng 123))
(for i 0 75
  (test-rng (math/rng (:int seedrng))))

(assert (deep-not= (-> 123 math/rng (:buffer 16))
                   (-> 456 math/rng (:buffer 16))) "math/rng-buffer 1")

(assert-no-error "math/rng-buffer 2" (math/seedrandom "abcdefg"))

(end-suite)

