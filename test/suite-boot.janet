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

# Let

(assert (= (let [a 1 b 2] (+ a b)) 3) "simple let")
(assert (= (let [[a b] @[1 2]] (+ a b)) 3) "destructured let")
(assert (= (let [[a [c d] b] @[1 (tuple 4 3) 2]] (+ a b c d)) 10)
        "double destructured let")

# Macros

(defn dub [x] (+ x x))
(assert (= 2 (dub 1)) "defn macro")
(do
  (defn trip [x] (+ x x x))
  (assert (= 3 (trip 1)) "defn macro triple"))
(do
  (var i 0)
  (when true
    (++ i)
    (++ i)
    (++ i)
    (++ i)
    (++ i)
    (++ i))
  (assert (= i 6) "when macro"))

# Add truthy? to core - ded08b6
(assert (= true ;(map truthy? [0 "" true @{} {} [] '()])) "truthy values")
(assert (= false ;(map truthy? [nil false])) "non-truthy values")

## Polymorphic comparison -- Issue #272

# confirm polymorphic comparison delegation to primitive comparators:
(assert (= 0 (cmp 3 3)) "compare-primitive integers (1)")
(assert (= -1 (cmp 3 5)) "compare-primitive integers (2)")
(assert (= 1 (cmp "foo" "bar")) "compare-primitive strings")
(assert (= 0 (compare 1 1)) "compare integers (1)")
(assert (= -1 (compare 1 2)) "compare integers (2)")
(assert (= 1 (compare "foo" "bar")) "compare strings (1)")

(assert (compare< 1 2 3 4 5 6) "compare less than integers")
(assert (not (compare> 1 2 3 4 5 6)) "compare not greater than integers")
(assert (compare< 1.0 2.0 3.0 4.0 5.0 6.0) "compare less than reals")
(assert (compare> 6 5 4 3 2 1) "compare greater than integers")
(assert (compare> 6.0 5.0 4.0 3.0 2.0 1.0) "compare greater than reals")
(assert (not (compare< 6.0 5.0 4.0 3.0 2.0 1.0)) "compare less than reals")
(assert (compare<= 1 2 3 3 4 5 6) "compare less than or equal to integers")
(assert (compare<= 1.0 2.0 3.0 3.0 4.0 5.0 6.0)
        "compare less than or equal to reals")
(assert (compare>= 6 5 4 4 3 2 1)
        "compare greater than or equal to integers")
(assert (compare>= 6.0 5.0 4.0 4.0 3.0 2.0 1.0)
        "compare greater than or equal to reals")
(assert (compare< 1.0 nil false true
           (fiber/new (fn [] 1))
           "hi"
           (quote hello)
           :hello
           (array 1 2 3)
           (tuple 1 2 3)
           (table "a" "b" "c" "d")
           (struct 1 2 3 4)
           (buffer "hi")
           (fn [x] (+ x x))
           print) "compare type ordering")

# test polymorphic compare with 'objects' (table/setproto)
(def mynum
  @{:type :mynum :v 0 :compare
    (fn [self other]
      (case (type other)
      :number (cmp (self :v) other)
      :table (when (= (get other :type) :mynum)
               (cmp (self :v) (other :v)))))})

(let [n3 (table/setproto @{:v 3} mynum)]
  (assert (= 0 (compare 3 n3)) "compare num to object (1)")
  (assert (= -1 (compare n3 4)) "compare object to num (2)")
  (assert (= 1 (compare (table/setproto @{:v 4} mynum) n3))
          "compare object to object")
  (assert (compare< 2 n3 4) "compare< poly")
  (assert (compare> 4 n3 2) "compare> poly")
  (assert (compare<= 2 3 n3 4) "compare<= poly")
  (assert (compare= 3 n3 (table/setproto @{:v 3} mynum)) "compare= poly")
  (assert (deep= (sorted @[4 5 n3 2] compare<) @[2 n3 4 5])
          "polymorphic sort"))

(let [MAX_INT_64_STRING "9223372036854775807"
      MAX_UINT_64_STRING "18446744073709551615"
      MAX_INT_IN_DBL_STRING "9007199254740991"
      NAN (math/log -1)
      INF (/ 1 0)
      MINUS_INF (/ -1 0)
      compare-poly-tests
      [[(int/s64 3) (int/u64 3) 0]
       [(int/s64 -3) (int/u64 3) -1]
       [(int/s64 3) (int/u64 2) 1]
       [(int/s64 3) 3 0] [(int/s64 3) 4 -1] [(int/s64 3) -9 1]
       [(int/u64 3) 3 0] [(int/u64 3) 4 -1] [(int/u64 3) -9 1]
       [3 (int/s64 3) 0] [3 (int/s64 4) -1] [3 (int/s64 -5) 1]
       [3 (int/u64 3) 0] [3 (int/u64 4) -1] [3 (int/u64 2) 1]
       [(int/s64 MAX_INT_64_STRING) (int/u64 MAX_UINT_64_STRING) -1]
       [(int/s64 MAX_INT_IN_DBL_STRING)
        (scan-number MAX_INT_IN_DBL_STRING) 0]
       [(int/u64 MAX_INT_IN_DBL_STRING)
        (scan-number MAX_INT_IN_DBL_STRING) 0]
       [(+ 1 (int/u64 MAX_INT_IN_DBL_STRING))
        (scan-number MAX_INT_IN_DBL_STRING) 1]
       [(int/s64 0) INF -1] [(int/u64 0) INF -1]
       [MINUS_INF (int/u64 0) -1] [MINUS_INF (int/s64 0) -1]
       [(int/s64 1) NAN 0] [NAN (int/u64 1) 0]]]
  (each [x y c] compare-poly-tests
    (assert (= c (compare x y))
            (string/format "compare polymorphic %q %q %d" x y c))))

# Add any? predicate to core - 7478ad11
(assert (= nil (any? [])) "any? 1")
(assert (= nil (any? [false nil])) "any? 2")
(assert (= nil (any? [nil false])) "any? 3")
(assert (= 1 (any? [1])) "any? 4")
(assert (nan? (any? [nil math/nan nil])) "any? 5")
(assert (= true
           (any? [nil nil false nil nil true nil nil nil nil false :a nil]))
        "any? 6")

# Some higher order functions and macros

(def my-array @[1 2 3 4 5 6])
(def x (if-let [x (get my-array 5)] x))
(assert (= x 6) "if-let")
(def x (if-let [y (get @{} :key)] 10 nil))
(assert (not x) "if-let 2")

(assert (= 14 (sum (map inc @[1 2 3 4]))) "sum map")
(def myfun (juxt + - * /))
(assert (= [2 -2 2 0.5] (myfun 2)) "juxt")

# Case statements
(assert
  (= :six (case (+ 1 2 3)
            1 :one
            2 :two
            3 :three
            4 :four
            5 :five
            6 :six
            7 :seven
            8 :eight
            9 :nine)) "case macro")

(assert (= 7 (case :a :b 5 :c 6 :u 10 7)) "case with default")

# Testing the seq, catseq, and loop macros
(def xs (apply tuple (seq [x :range [0 10] :when (even? x)]
                       (tuple (/ x 2) x))))
(assert (= xs '((0 0) (1 2) (2 4) (3 6) (4 8))) "seq macro 1")

(def xs (apply tuple (seq [x :down [8 -2] :when (even? x)]
                       (tuple (/ x 2) x))))
(assert (= xs '((4 8) (3 6) (2 4) (1 2) (0 0))) "seq macro 2")

(def xs (catseq [x :range [0 3]] [x x]))
(assert (deep= xs @[0 0 1 1 2 2]) "catseq")

# :range-to and :down-to
(assert (deep= (seq [x :range-to [0 10]] x) (seq [x :range [0 11]] x))
        "loop :range-to")
(assert (deep= (seq [x :down-to [10 0]] x) (seq [x :down [10 -1]] x))
        "loop :down-to")

(def res @{})
(loop [[k v] :pairs @{1 2 3 4 5 6}]
  (put res k v))
(assert (and
          (= (get res 1) 2)
          (= (get res 3) 4)
          (= (get res 5) 6)) "loop :pairs")

# Generators
(def gen (generate [x :range [0 100] :when (pos? (% x 4))] x))
(var gencount 0)
(loop [x :in gen]
  (++ gencount)
  (assert (pos? (% x 4)) "generate in loop"))
(assert (= gencount 75) "generate loop count")

# Even and odd

(assert (odd? 9) "odd? 1")
(assert (odd? -9) "odd? 2")
(assert (not (odd? 10)) "odd? 3")
(assert (not (odd? 0)) "odd? 4")
(assert (not (odd? -10)) "odd? 5")
(assert (not (odd? 1.1)) "odd? 6")
(assert (not (odd? -0.1)) "odd? 7")
(assert (not (odd? -1.1)) "odd? 8")
(assert (not (odd? -1.6)) "odd? 9")

(assert (even? 10) "even? 1")
(assert (even? -10) "even? 2")
(assert (even? 0) "even? 3")
(assert (not (even? 9)) "even? 4")
(assert (not (even? -9)) "even? 5")
(assert (not (even? 0.1)) "even? 6")
(assert (not (even? -0.1)) "even? 7")
(assert (not (even? -10.1)) "even? 8")
(assert (not (even? -10.6)) "even? 9")

# Map arities
(assert (deep= (map inc [1 2 3]) @[2 3 4]))
(assert (deep= (map + [1 2 3] [10 20 30]) @[11 22 33]))
(assert (deep= (map + [1 2 3] [10 20 30] [100 200 300]) @[111 222 333]))
(assert (deep= (map + [1 2 3] [10 20 30] [100 200 300] [1000 2000 3000])
               @[1111 2222 3333]))
(assert (deep= (map +
                    [1 2 3] [10 20 30] [100 200 300] [1000 2000 3000]
                    [10000 20000 30000])
               @[11111 22222 33333]))

# Mapping uses the shortest sequence
(assert (deep= (map + [1 2 3 4] [10 20 30]) @[11 22 33]))
(assert (deep= (map + [1 2 3 4] [10 20 30] [100 200]) @[111 222]))
(assert (deep= (map + [1 2 3 4] [10 20 30] [100 200] [1000]) @[1111]))

(assert (= false (deep-not= [1] [1])) "issue #1149")

# Sort function
(assert (deep=
          (range 99)
          (sort (mapcat (fn [[x y z]] [z y x]) (partition 3 (range 99)))))
        "sort 5")
(assert (<= ;(sort (map (fn [x] (math/random)) (range 1000)))) "sort 6")

# And and or

(assert (= (and true true) true) "and true true")
(assert (= (and true false) false) "and true false")
(assert (= (and false true) false) "and false true")
(assert (= (and true true true) true) "and true true true")
(assert (= (and 0 1 2) 2) "and 0 1 2")
(assert (= (and 0 1 nil) nil) "and 0 1 nil")
(assert (= (and 1) 1) "and 1")
(assert (= (and) true) "and with no arguments")
(assert (= (and 1 true) true) "and with trailing true")
(assert (= (and 1 true 2) 2) "and with internal true")

(assert (= (or true true) true) "or true true")
(assert (= (or true false) true) "or true false")
(assert (= (or false true) true) "or false true")
(assert (= (or false false) false) "or false true")
(assert (= (or true true false) true) "or true true false")
(assert (= (or 0 1 2) 0) "or 0 1 2")
(assert (= (or nil 1 2) 1) "or nil 1 2")
(assert (= (or 1) 1) "or 1")
(assert (= (or) nil) "or with no arguments")

(assert (= (length (range 10)) 10) "(range 10)")
(assert (= (length (range 1 10)) 9) "(range 1 10)")
(assert (deep= @{:a 1 :b 2 :c 3} (zipcoll '[:a :b :c] '[1 2 3])) "zipcoll")

(def- a 100)
(assert (= a 100) "def-")

(assert (= :first
          (match @[1 3 5]
                 @[x y z] :first
                 :second)) "match 1")

(def val1 :avalue)
(assert (= :second
          (match val1
                 @[x y z] :first
                 :avalue :second
                 :third)) "match 2")

(assert (= 100
           (match @[50 40]
                  @[x x] (* x 3)
                  @[x y] (+ x y 10)
                  0)) "match 3")

# Some macros

(assert (= 2 (if-not 1 3 2)) "if-not 1")
(assert (= 3 (if-not false 3)) "if-not 2")
(assert (= 3 (if-not nil 3 2)) "if-not 3")
(assert (= nil (if-not true 3)) "if-not 4")

(assert (= 4 (unless false (+ 1 2 3) 4)) "unless")

(end-suite)

