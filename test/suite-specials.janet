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

# Regression Test #137
(def [a b c] (range 10))
(assert (= a 0) "regression #137 (1)")
(assert (= b 1) "regression #137 (2)")
(assert (= c 2) "regression #137 (3)")

(var [x y z] (range 10))
(assert (= x 0) "regression #137 (4)")
(assert (= y 1) "regression #137 (5)")
(assert (= z 2) "regression #137 (6)")

# Test destructuring
(do
  (def test-tab @{:a 1 :b 2})
  (def {:a a :b b} test-tab)
  (assert (= a 1) "dictionary destructuring 1")
  (assert (= b 2) "dictionary destructuring 2"))
(do
  (def test-tab @{'a 1 'b 2 3 4})
  (def {'a a 'b b (+ 1 2) c} test-tab)
  (assert (= a 1) "dictionary destructuring 3")
  (assert (= b 2) "dictionary destructuring 4")
  (assert (= c 4) "dictionary destructuring 5 - expression as key"))
(let [test-tuple [:a :b 1 2]]
  (def [a b one two] test-tuple)
  (assert (= a :a) "tuple destructuring 1")
  (assert (= b :b) "tuple destructuring 2")
  (assert (= two 2) "tuple destructuring 3"))
(let [test-tuple [:a :b 1 2]]
  (def [a & rest] test-tuple)
  (assert (= a :a) "tuple destructuring 4 - rest")
  (assert (= rest [:b 1 2]) "tuple destructuring 5 - rest"))
(do
  (def [a b & rest] [:a :b nil :d])
  (assert (= a :a) "tuple destructuring 6 - rest")
  (assert (= b :b) "tuple destructuring 7 - rest")
  (assert (= rest [nil :d]) "tuple destructuring 8 - rest"))
(do
  (def [[a b] x & rest] [[1 2] :a :c :b :a])
  (assert (= a 1) "tuple destructuring 9 - rest")
  (assert (= b 2) "tuple destructuring 10 - rest")
  (assert (= x :a) "tuple destructuring 11 - rest")
  (assert (= rest [:c :b :a]) "tuple destructuring 12 - rest"))
(do
  (def [a b & rest] [:a :b])
  (assert (= a :a) "tuple destructuring 13 - rest")
  (assert (= b :b) "tuple destructuring 14 - rest")
  (assert (= rest []) "tuple destructuring 15 - rest"))

(do
  (def [[a b & r1] c & r2] [[:a :b 1 2] :c 3 4])
  (assert (= a :a) "tuple destructuring 16 - rest")
  (assert (= b :b) "tuple destructuring 17 - rest")
  (assert (= c :c) "tuple destructuring 18 - rest")
  (assert (= r1 [1 2]) "tuple destructuring 19 - rest")
  (assert (= r2 [3 4]) "tuple destructuring 20 - rest"))

# Metadata

(def foo-with-tags :a-tag :bar)
(assert (get (dyn 'foo-with-tags) :a-tag)
        "extra keywords in def are metadata tags")

(def foo-with-meta {:baz :quux} :bar)
(assert (= :quux (get (dyn 'foo-with-meta) :baz))
        "extra struct in def is metadata")

(defn foo-fn-with-meta {:baz :quux}
  "This is a function"
  [x]
  (identity x))
(assert (= :quux (get (dyn 'foo-fn-with-meta) :baz))
        "extra struct in defn is metadata")
(assert (= "(foo-fn-with-meta x)\n\nThis is a function"
           (get (dyn 'foo-fn-with-meta) :doc))
        "extra string in defn is docstring")

(end-suite)

