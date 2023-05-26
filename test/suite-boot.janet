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

# Let - 807f981

(assert (= (let [a 1 b 2] (+ a b)) 3) "simple let")
(assert (= (let [[a b] @[1 2]] (+ a b)) 3) "destructured let")
(assert (= (let [[a [c d] b] @[1 (tuple 4 3) 2]] (+ a b c d)) 10)
        "double destructured let")

# Macros - b305a7c

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

# Some higher order functions and macros - 5e2de33

(def my-array @[1 2 3 4 5 6])
(def x (if-let [x (get my-array 5)] x))
(assert (= x 6) "if-let")
(def x (if-let [y (get @{} :key)] 10 nil))
(assert (not x) "if-let 2")

(assert (= 14 (sum (map inc @[1 2 3 4]))) "sum map")
(def myfun (juxt + - * /))
(assert (= [2 -2 2 0.5] (myfun 2)) "juxt")

# Case statements - 5249228
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

# Testing the seq, tabseq, catseq, and loop macros
(def xs (apply tuple (seq [x :range [0 10] :when (even? x)]
                       (tuple (/ x 2) x))))
(assert (= xs '((0 0) (1 2) (2 4) (3 6) (4 8))) "seq macro 1")

(def xs (apply tuple (seq [x :down [8 -2] :when (even? x)]
                       (tuple (/ x 2) x))))
(assert (= xs '((4 8) (3 6) (2 4) (1 2) (0 0))) "seq macro 2")

(assert (deep= (tabseq [i :in (range 3)] i (* 3 i))
               @{0 0 1 3 2 6}))

(assert (deep= (tabseq [i :in (range 3)] i)
               @{}))

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

# Issue 428
(var result nil)
(defn f [] (yield {:a :ok}))
(assert-no-error "issue 428 1"
                 (loop [{:a x} :in (fiber/new f)] (set result x)))
(assert (= result :ok) "issue 428 2")

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

# And/or checks

(assert (= false (and false false)) "and 1")
(assert (= false (or false false)) "or 1")

# Range
(assert (deep= (range 10) @[0 1 2 3 4 5 6 7 8 9]) "range 1 argument")
(assert (deep= (range 5 10) @[5 6 7 8 9]) "range 2 arguments")
(assert (deep= (range 5 10 2) @[5 7 9]) "range 3 arguments")
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

# Match checks

(assert (= :hi (match nil nil :hi)) "match 1")
(assert (= :hi (match {:a :hi} {:a a} a)) "match 2")
(assert (= nil (match {:a :hi} {:a a :b b} a)) "match 3")
(assert (= nil (match [1 2] [a b c] a)) "match 4")
(assert (= 2 (match [1 2] [a b] b)) "match 5")
(assert (= [2 :a :b] (match [1 2 :a :b] [o & rest] rest)) "match 6")
(assert (= [] (match @[:a] @[x & r] r :fallback)) "match 7")
(assert (= :fallback (match @[1] @[x y & r] r :fallback)) "match 8")
(assert (= [1 2 3 4] (match @[1 2 3 4] @[x y z & r] [x y z ;r] :fallback))
        "match 9")

# Test cases for #293
(assert (= :yes (match [1 2 3] [_ a _] :yes :no)) "match wildcard 1")
(assert (= :no (match [1 2 3] [__ a __] :yes :no)) "match wildcard 2")
(assert (= :yes (match [1 2 [1 2 3]] [_ a [_ _ _]] :yes :no))
        "match wildcard 3")
(assert (= :yes (match [1 2 3] (_ (even? 2)) :yes :no)) "match wildcard 4")
(assert (= :yes (match {:a 1} {:a _} :yes :no)) "match wildcard 5")
(assert (= false (match {:a 1 :b 2 :c 3}
                   {:a a :b _ :c _ :d _} :no
                   {:a _ :b _ :c _} false
                   :no)) "match wildcard 6")
(assert (= nil (match {:a 1 :b 2 :c 3}
                 {:a a :b _ :c _ :d _} :no
                 {:a _ :b _ :c _} nil
                 :no)) "match wildcard 7")
(assert (= "t" (match [true nil] [true _] "t")) "match wildcard 8")

# quoted match test
(assert (= :yes (match 'john 'john :yes _ :nope)) "quoted literal match 1")
(assert (= :nope (match 'john ''john :yes _ :nope)) "quoted literal match 2")

# Some macros

(assert (= 2 (if-not 1 3 2)) "if-not 1")
(assert (= 3 (if-not false 3)) "if-not 2")
(assert (= 3 (if-not nil 3 2)) "if-not 3")
(assert (= nil (if-not true 3)) "if-not 4")

(assert (= 4 (unless false (+ 1 2 3) 4)) "unless")

# take

(assert (deep= (take 0 []) []) "take 1")
(assert (deep= (take 10 []) []) "take 2")
(assert (deep= (take 0 [1 2 3 4 5]) []) "take 3")
(assert (deep= (take 10 [1 2 3]) [1 2 3]) "take 4")
(assert (deep= (take -1 [:a :b :c]) []) "take 5")
(assert (deep= (take 3 (generate [x :in [1 2 3 4 5]] x)) @[1 2 3])
        "take from fiber")
# NB: repeatedly resuming a fiber created with `generate` includes a `nil`
# as the final element. Thus a generate of 2 elements will create an array
# of 3.
(assert (= (length (take 4 (generate [x :in [1 2]] x))) 2)
        "take from short fiber")

# take-until

(assert (deep= (take-until pos? @[]) []) "take-until 1")
(assert (deep= (take-until pos? @[1 2 3]) []) "take-until 2")
(assert (deep= (take-until pos? @[-1 -2 -3]) [-1 -2 -3]) "take-until 3")
(assert (deep= (take-until pos? @[-1 -2 3]) [-1 -2]) "take-until 4")
(assert (deep= (take-until pos? @[-1 1 -2]) [-1]) "take-until 5")
(assert (deep= (take-until |(= $ 115) "books") "book") "take-until 6")
(assert (deep= (take-until |(= $ 115) (generate [x :in "books"] x))
               @[98 111 111 107]) "take-until from fiber")

# take-while

(assert (deep= (take-while neg? @[]) []) "take-while 1")
(assert (deep= (take-while neg? @[1 2 3]) []) "take-while 2")
(assert (deep= (take-while neg? @[-1 -2 -3]) [-1 -2 -3]) "take-while 3")
(assert (deep= (take-while neg? @[-1 -2 3]) [-1 -2]) "take-while 4")
(assert (deep= (take-while neg? @[-1 1 -2]) [-1]) "take-while 5")
(assert (deep= (take-while neg? (generate [x :in  @[-1 1 -2]] x))
               @[-1]) "take-while from fiber")

# drop

(assert (deep= (drop 0 []) []) "drop 1")
(assert (deep= (drop 10 []) []) "drop 2")
(assert (deep= (drop 0 [1 2 3 4 5]) [1 2 3 4 5]) "drop 3")
(assert (deep= (drop 10 [1 2 3]) []) "drop 4")
(assert (deep= (drop -1 [1 2 3]) [1 2]) "drop 5")
(assert (deep= (drop -10 [1 2 3]) []) "drop 6")
(assert (deep= (drop 1 "abc") "bc") "drop 7")
(assert (deep= (drop 10 "abc") "") "drop 8")
(assert (deep= (drop -1 "abc") "ab") "drop 9")
(assert (deep= (drop -10 "abc") "") "drop 10")
(assert-error :invalid-type (drop 3 {}) "drop 11")

# drop-until

(assert (deep= (drop-until pos? @[]) []) "drop-until 1")
(assert (deep= (drop-until pos? @[1 2 3]) [1 2 3]) "drop-until 2")
(assert (deep= (drop-until pos? @[-1 -2 -3]) []) "drop-until 3")
(assert (deep= (drop-until pos? @[-1 -2 3]) [3]) "drop-until 4")
(assert (deep= (drop-until pos? @[-1 1 -2]) [1 -2]) "drop-until 5")
(assert (deep= (drop-until |(= $ 115) "books") "s") "drop-until 6")

# Comment macro
(comment 1)
(comment 1 2)
(comment 1 2 3)
(comment 1 2 3 4)

# comp should be variadic
(assert (= 10 ((comp +) 1 2 3 4)) "variadic comp 1")
(assert (= 11 ((comp inc +) 1 2 3 4)) "variadic comp 2")
(assert (= 12 ((comp inc inc +) 1 2 3 4)) "variadic comp 3")
(assert (= 13 ((comp inc inc inc +) 1 2 3 4)) "variadic comp 4")
(assert (= 14 ((comp inc inc inc inc +) 1 2 3 4)) "variadic comp 5")
(assert (= 15 ((comp inc inc inc inc inc +) 1 2 3 4)) "variadic comp 6")
(assert (= 16 ((comp inc inc inc inc inc inc +) 1 2 3 4))
        "variadic comp 7")

# Function shorthand
(assert (= (|(+ 1 2 3)) 6) "function shorthand 1")
(assert (= (|(+ 1 2 3 $) 4) 10) "function shorthand 2")
(assert (= (|(+ 1 2 3 $0) 4) 10) "function shorthand 3")
(assert (= (|(+ $0 $0 $0 $0) 4) 16) "function shorthand 4")
(assert (= (|(+ $ $ $ $) 4) 16) "function shorthand 5")
(assert (= (|4) 4) "function shorthand 6")
(assert (= (((|||4))) 4) "function shorthand 7")
(assert (= (|(+ $1 $1 $1 $1) 2 4) 16) "function shorthand 8")
(assert (= (|(+ $0 $1 $3 $2 $6) 0 1 2 3 4 5 6) 12) "function shorthand 9")
(assert (= (|(+ $0 $99) ;(range 100)) 99) "function shorthand 10")

(defn idx= [x y] (= (tuple/slice x) (tuple/slice y)))

# Simple take, drop, etc. tests.
(assert (idx= (take 10 (range 100)) (range 10)) "take 10")
(assert (idx= (drop 10 (range 100)) (range 10 100)) "drop 10")

# with-vars
(var abc 123)
(assert (= 356 (with-vars [abc 456] (- abc 100))) "with-vars 1")
(assert-error "with-vars 2" (with-vars [abc 456] (error :oops)))
(assert (= abc 123) "with-vars 3")

# Top level unquote
(defn constantly
  []
  (comptime (math/random)))

(assert (= (constantly) (constantly)) "comptime 1")

(assert-error "arity issue in macro" (eval '(each [])))
(assert-error "comptime issue" (eval '(comptime (error "oops"))))

(var counter 0)
(when-with [x nil |$]
           (++ counter))
(when-with [x 10 |$]
           (+= counter 10))

(assert (= 10 counter) "when-with 1")

(if-with [x nil |$] (++ counter) (+= counter 10))
(if-with [x true |$] (+= counter 20) (+= counter 30))

(assert (= 40 counter) "if-with 1")

(def a @[])
(eachk x [:a :b :c :d]
  (array/push a x))
(assert (deep= (range 4) a) "eachk 1")

(with-dyns [:err @""]
  (tracev (def my-unique-var-name true))
  (assert my-unique-var-name "tracev upscopes"))

# Prompts and Labels

(assert (= 10 (label a (for i 0 10 (if (= i 5) (return a 10))))) "label 1")

(defn recur
  [lab x y]
  (when (= x y) (return lab :done))
  (def res (label newlab (recur (or lab newlab) (+ x 1) y)))
  (if lab :oops res))
(assert (= :done (recur nil 0 10)) "label 2")

(assert (= 10 (prompt :a (for i 0 10 (if (= i 5) (return :a 10)))))
        "prompt 1")

(defn- inner-loop
  [i]
  (if (= i 5)
    (return :a 10)))

(assert (= 10 (prompt :a (for i 0 10 (inner-loop i)))) "prompt 2")

(defn- inner-loop2
  [i]
  (try
    (if (= i 5)
      (error 10))
    ([err] (return :a err))))

(assert (= 10 (prompt :a (for i 0 10 (inner-loop2 i)))) "prompt 3")

# chr
(assert (= (chr "a") 97) "chr 1")

# Reduce2

(assert (= (reduce + 0 (range 1 10)) (reduce2 + (range 10))) "reduce2 1")
(assert (= (reduce * 1 (range 2 10)) (reduce2 * (range 1 10))) "reduce2 2")
(assert (= nil (reduce2 * [])) "reduce2 3")

# Accumulate

(assert (deep= (accumulate + 0 (range 5)) @[0 1 3 6 10]) "accumulate 1")
(assert (deep= (accumulate2 + (range 5)) @[0 1 3 6 10]) "accumulate2 1")
(assert (deep= @[] (accumulate2 + [])) "accumulate2 2")
(assert (deep= @[] (accumulate 0 + [])) "accumulate 2")

# in vs get regression - #340
(assert (nil? (first @"")) "in vs get 1")
(assert (nil? (last @"")) "in vs get 1")

# index-of
(assert (= nil (index-of 10 [])) "index-of 1")
(assert (= nil (index-of 10 [1 2 3])) "index-of 2")
(assert (= 1 (index-of 2 [1 2 3])) "index-of 3")
(assert (= 0 (index-of :a [:a :b :c])) "index-of 4")
(assert (= nil (index-of :a {})) "index-of 5")
(assert (= :a (index-of :A {:a :A :b :B})) "index-of 6")
(assert (= :a (index-of :A @{:a :A :b :B})) "index-of 7")
(assert (= 0 (index-of (chr "a") "abc")) "index-of 8")
(assert (= nil (index-of (chr "a") "")) "index-of 9")
(assert (= nil (index-of 10 @[])) "index-of 10")
(assert (= nil (index-of 10 @[1 2 3])) "index-of 11")

# Regression
(assert (= {:x 10} (|(let [x $] ~{:x ,x}) 10)) "issue 463")

# macex testing
(assert (deep= (macex1 '~{1 2 3 4}) '~{1 2 3 4}) "macex1 qq struct")
(assert (deep= (macex1 '~@{1 2 3 4}) '~@{1 2 3 4}) "macex1 qq table")
(assert (deep= (macex1 '~(1 2 3 4)) '~[1 2 3 4]) "macex1 qq tuple")
(assert (= :brackets (tuple/type (1 (macex1 '~[1 2 3 4]))))
        "macex1 qq bracket tuple")
(assert (deep= (macex1 '~@[1 2 3 4 ,blah]) '~@[1 2 3 4 ,blah])
        "macex1 qq array")

# Sourcemaps in threading macros
(defn check-threading [macro expansion]
  (def expanded (macex1 (tuple macro 0 '(x) '(y))))
  (assert (= expanded expansion) (string macro " expansion value"))
  (def smap-x (tuple/sourcemap (get expanded 1)))
  (def smap-y (tuple/sourcemap expanded))
  (def line first)
  (defn column [t] (t 1))
  (assert (not= smap-x [-1 -1]) (string macro " x sourcemap existence"))
  (assert (not= smap-y [-1 -1]) (string macro " y sourcemap existence"))
  (assert (or (< (line smap-x) (line smap-y))
              (and (= (line smap-x) (line smap-y))
                   (< (column smap-x) (column smap-y))))
          (string macro " relation between x and y sourcemap")))

(check-threading '-> '(y (x 0)))
(check-threading '->> '(y (x 0)))

# keep-syntax
(let [brak '[1 2 3]
      par '(1 2 3)]

  (tuple/setmap brak 2 1)

  (assert (deep= (keep-syntax brak @[1 2 3]) @[1 2 3])
          "keep-syntax brackets ignore array")
  (assert (= (keep-syntax! brak @[1 2 3]) '[1 2 3])
          "keep-syntax! brackets replace array")

  (assert (= (keep-syntax! par (map inc @[1 2 3])) '(2 3 4))
          "keep-syntax! parens coerce array")
  (assert (not= (keep-syntax! brak @[1 2 3]) '(1 2 3))
          "keep-syntax! brackets not parens")
  (assert (not= (keep-syntax! par @[1 2 3]) '[1 2 3])
          "keep-syntax! parens not brackets")
  (assert (= (tuple/sourcemap brak)
             (tuple/sourcemap (keep-syntax! brak @[1 2 3])))
          "keep-syntax! brackets source map")

  (keep-syntax par brak)
  (assert (not= (tuple/sourcemap brak) (tuple/sourcemap par))
          "keep-syntax no mutate")
  (assert (= (keep-syntax 1 brak) brak) "keep-syntax brackets ignore type"))

# Curenv - 28439d822, f7c556e
(assert (= (curenv) (curenv 0)) "curenv 1")
(assert (= (table/getproto (curenv)) (curenv 1)) "curenv 2")
(assert (= nil (curenv 1000000)) "curenv 3")
(assert (= root-env (curenv 1)) "curenv 4")

# Import macro test
(assert-no-error "import macro 1" (macex '(import a :as b :fresh maybe)))
(assert (deep= ~(,import* "a" :as "b" :fresh maybe)
               (macex '(import a :as b :fresh maybe))) "import macro 2")

# #477 walk preserving bracket type
(assert (= :brackets (tuple/type (postwalk identity '[])))
        "walk square brackets 1")
(assert (= :brackets (tuple/type (walk identity '[])))
        "walk square brackets 2")

# Issue #751
(def t {:side false})
(assert (nil? (get-in t [:side :note])) "get-in with false value")
(assert (= (get-in t [:side :note] "dflt") "dflt")
        "get-in with false value and default")

# Evaluate stream with `dofile`
(def [r w] (os/pipe))
(:write w "(setdyn :x 10)")
(:close w)
(def stream-env (dofile r))
(assert (= (stream-env :x) 10) "dofile stream 1")

# Test thaw and freeze
(def table-to-freeze @{:c 22 :b [1 2 3 4] :d @"test" :e "test2"})
(def table-to-freeze-with-inline-proto
  @{:a @[1 2 3] :b @[1 2 3 4] :c 22 :d @"test" :e @"test2"})
(def struct-to-thaw
  (struct/with-proto {:a [1 2 3]} :c 22 :b [1 2 3 4] :d "test" :e "test2"))
(table/setproto table-to-freeze @{:a @[1 2 3]})

(assert (deep= {:a [1 2 3] :b [1 2 3 4] :c 22 :d "test" :e "test2"}
               (freeze table-to-freeze)))
(assert (deep= table-to-freeze-with-inline-proto (thaw table-to-freeze)))
(assert (deep= table-to-freeze-with-inline-proto (thaw struct-to-thaw)))

(var counter 0)
(def thunk (delay (++ counter)))
(assert (= (thunk) 1) "delay 1")
(assert (= counter 1) "delay 2")
(assert (= (thunk) 1) "delay 3")
(assert (= counter 1) "delay 4")

(end-suite)

