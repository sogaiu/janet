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
(start-suite 0)

(assert (= 10 (+ 1 2 3 4)) "addition")
(assert (= -8 (- 1 2 3 4)) "subtraction")
(assert (= 24 (* 1 2 3 4)) "multiplication")
(assert (= 4 (blshift 1 2)) "left shift")
(assert (= 1 (brshift 4 2)) "right shift")
(assert (< 1 2 3 4 5 6) "less than integers")
(assert (< 1.0 2.0 3.0 4.0 5.0 6.0) "less than reals")
(assert (> 6 5 4 3 2 1) "greater than integers")
(assert (> 6.0 5.0 4.0 3.0 2.0 1.0) "greater than reals")
(assert (<= 1 2 3 3 4 5 6) "less than or equal to integers")
(assert (<= 1.0 2.0 3.0 3.0 4.0 5.0 6.0) "less than or equal to reals")
(assert (>= 6 5 4 4 3 2 1) "greater than or equal to integers")
(assert (>= 6.0 5.0 4.0 4.0 3.0 2.0 1.0) "greater than or equal to reals")
(assert (= 7 (% 20 13)) "modulo 1")
(assert (= -7 (% -20 13)) "modulo 2")

(assert (< 1.0 nil false true
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
           print) "type ordering")

(assert (= (string (buffer "123" "456")) (string @"123456")) "buffer literal")
(assert (= (get {} 1) nil) "get nil from empty struct")
(assert (= (get @{} 1) nil) "get nil from empty table")
(assert (= (get {:boop :bap} :boop) :bap) "get non nil from struct")
(assert (= (get @{:boop :bap} :boop) :bap) "get non nil from table")
(assert (= (get @"\0" 0) 0) "get non nil from buffer")
(assert (= (get @"\0" 1) nil) "get nil from buffer oob")
(assert (put @{} :boop :bap) "can add to empty table")
(assert (put @{1 3} :boop :bap) "can add to non-empty table")

(assert (not false) "false literal")
(assert true "true literal")
(assert (not nil) "nil literal")
(assert (= 7 (bor 3 4)) "bit or")
(assert (= 0 (band 3 4)) "bit and")
(assert (= 0xFF (bxor 0x0F 0xF0)) "bit xor")
(assert (= 0xF0 (bxor 0xFF 0x0F)) "bit xor 2")

# Set global variables to prevent some possible compiler optimizations that defeat point of the test
(var zero 0)
(var one 1)
(var two 2)
(var three 3)
(var plus +)
(assert (= 22 (plus one (plus 1 2 two) (plus 8 (plus zero 1) 4 three))) "nested function calls")

# String literals
(assert (= "abcd" "\x61\x62\x63\x64") "hex escapes")
(assert (= "\e" "\x1B") "escape character")
(assert (= "\x09" "\t") "tab character")

# McCarthy's 91 function
(var f91 nil)
(set f91 (fn [n] (if (> n 100) (- n 10) (f91 (f91 (+ n 11))))))
(assert (= 91 (f91 10)) "f91(10) = 91")
(assert (= 91 (f91 11)) "f91(11) = 91")
(assert (= 91 (f91 20)) "f91(20) = 91")
(assert (= 91 (f91 31)) "f91(31) = 91")
(assert (= 91 (f91 100)) "f91(100) = 91")
(assert (= 91 (f91 101)) "f91(101) = 91")
(assert (= 92 (f91 102)) "f91(102) = 92")
(assert (= 93 (f91 103)) "f91(103) = 93")
(assert (= 94 (f91 104)) "f91(104) = 94")

# Fibonacci
(def fib (do (var fib nil) (set fib (fn [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))))
(def fib2 (fn fib2 [n] (if (< n 2) n (+ (fib2 (- n 1)) (fib2 (- n 2))))))

(assert (= (fib 0) (fib2 0) 0) "fib(0)")
(assert (= (fib 1) (fib2 1) 1) "fib(1)")
(assert (= (fib 2) (fib2 2) 1) "fib(2)")
(assert (= (fib 3) (fib2 3) 2) "fib(3)")
(assert (= (fib 4) (fib2 4) 3) "fib(4)")
(assert (= (fib 5) (fib2 5) 5) "fib(5)")
(assert (= (fib 6) (fib2 6) 8) "fib(6)")
(assert (= (fib 7) (fib2 7) 13) "fib(7)")
(assert (= (fib 8) (fib2 8) 21) "fib(8)")
(assert (= (fib 9) (fib2 9) 34) "fib(9)")
(assert (= (fib 10) (fib2 10) 55) "fib(10)")

# Closure in non function scope
(def outerfun (fn [x y]
                (def c (do
                         (def someval (+ 10 y))
                         (def ctemp (if x (fn [] someval) (fn [] y)))
                         ctemp
                         ))
                (+ 1 2 3 4 5 6 7)
                c))

(assert (= ((outerfun 1 2)) 12) "inner closure 1")
(assert (= ((outerfun nil 2)) 2) "inner closure 2")
(assert (= ((outerfun false 3)) 3) "inner closure 3")

(assert (= '(1 2 3) (quote (1 2 3)) (tuple 1 2 3)) "quote shorthand")

((fn []
   (var accum 1)
   (var count 0)
   (while (< count 16)
     (set accum (blshift accum 1))
     (set count (+ 1 count)))
   (assert (= accum 65536) "loop in closure")))

(var accum 1)
(var count 0)
(while (< count 16)
  (set accum (blshift accum 1))
  (set count (+ 1 count)))
(assert (= accum 65536) "loop globally")

(assert (= (struct 1 2 3 4 5 6 7 8) (struct 7 8 5 6 3 4 1 2)) "struct order does not matter 1")
(assert (= (struct
             :apple 1
             6 :bork
             '(1 2 3) 5)
           (struct
             6 :bork
             '(1 2 3) 5
             :apple 1)) "struct order does not matter 2")

# Symbol function

(assert (= (symbol "abc" 1 2 3) 'abc123) "symbol function")

# Fiber tests

(def afiber (fiber/new (fn []
                         (def x (yield))
                         (error (string "hello, " x))) :ye))

(resume afiber) # first resume to prime
(def afiber-result (resume afiber "world!"))

(assert (= afiber-result "hello, world!") "fiber error result")
(assert (= (fiber/status afiber) :error) "fiber error status")

# yield tests

(def t (fiber/new (fn [&] (yield 1) (yield 2) 3)))

(assert (= 1 (resume t)) "initial transfer to new fiber")
(assert (= 2 (resume t)) "second transfer to fiber")
(assert (= 3 (resume t)) "return from fiber")
(assert (= (fiber/status t) :dead) "finished fiber is dead")

# Var arg tests

(def vargf (fn [more] (apply + more)))

(assert (= 0 (vargf @[])) "var arg no arguments")
(assert (= 1 (vargf @[1])) "var arg no packed arguments")
(assert (= 3 (vargf @[1 2])) "var arg tuple size 1")
(assert (= 10 (vargf @[1 2 3 4])) "var arg tuple size 2, 2 normal args")
(assert (= 110 (vargf @[1 2 3 4 10 10 10 10 10 10 10 10 10 10])) "var arg large tuple")

# Higher order functions

(def compose (fn [f g] (fn [& xs] (f (apply g xs)))))

(def -+ (compose - +))
(def +- (compose + -))

(assert (= (-+ 1 2 3 4) -10) "compose - +")
(assert (= (+- 1 2 3 4) -8) "compose + -")
(assert (= ((compose -+ +-) 1 2 3 4) 8) "compose -+ +-")
(assert (= ((compose +- -+) 1 2 3 4) 10) "compose +- -+")

# UTF-8

#🐙🐙🐙🐙

(defn foo [Θa Θb Θc] 0)
(def 🦊 :fox)
(def 🐮 :cow)
(assert (= (string "🐼" 🦊 🐮) "🐼foxcow") "emojis 🙉 :)")
(assert (not= 🦊 "🦊") "utf8 strings are not symbols and vice versa")
(assert (= "\U01F637" "😷") "unicode escape 1")
(assert (= "\u2623" "\U002623" "☣") "unicode escape 2")
(assert (= "\u24c2" "\U0024c2" "Ⓜ") "unicode escape 3")
(assert (= "\u0061" "a") "unicode escape 4")

# Symbols with @ character

(def @ 1)
(assert (= @ 1) "@ symbol")
(def @-- 2)
(assert (= @-- 2) "@-- symbol")
(def @hey 3)
(assert (= @hey 3) "@hey symbol")

# Merge sort

# Imperative (and verbose) merge sort merge
(defn merge
  [xs ys]
  (def ret @[])
  (def xlen (length xs))
  (def ylen (length ys))
  (var i 0)
  (var j 0)
  # Main merge
  (while (if (< i xlen) (< j ylen))
    (def xi (get xs i))
    (def yj (get ys j))
    (if (< xi yj)
      (do (array/push ret xi) (set i (+ i 1)))
      (do (array/push ret yj) (set j (+ j 1)))))
  # Push rest of xs
  (while (< i xlen)
    (def xi (get xs i))
    (array/push ret xi)
    (set i (+ i 1)))
  # Push rest of ys
  (while (< j ylen)
    (def yj (get ys j))
    (array/push ret yj)
    (set j (+ j 1)))
  ret)

(assert (apply <= (merge @[1 3 5] @[2 4 6])) "merge sort merge 1")
(assert (apply <= (merge @[1 2 3] @[4 5 6])) "merge sort merge 2")
(assert (apply <= (merge @[1 3 5] @[2 4 6 6 6 9])) "merge sort merge 3")
(assert (apply <= (merge '(1 3 5) @[2 4 6 6 6 9])) "merge sort merge 4")

(assert (deep= @[1 2 3 4 5] (sort @[5 3 4 1 2])) "sort 1")
(assert (deep= @[{:a 1} {:a 4} {:a 7}] (sort-by |($ :a) @[{:a 4} {:a 7} {:a 1}])) "sort 2")
(assert (deep= @[1 2 3 4 5] (sorted [5 3 4 1 2])) "sort 3")
(assert (deep= @[{:a 1} {:a 4} {:a 7}] (sorted-by |($ :a) [{:a 4} {:a 7} {:a 1}])) "sort 4")

# Gensym tests

(assert (not= (gensym) (gensym)) "two gensyms not equal")
((fn []
   (def syms (table))
   (var count 0)
   (while (< count 128)
     (put syms (gensym) true)
     (set count (+ 1 count)))
   (assert (= (length syms) 128) "many symbols")))

# Let

(assert (= (let [a 1 b 2] (+ a b)) 3) "simple let")
(assert (= (let [[a b] @[1 2]] (+ a b)) 3) "destructured let")
(assert (= (let [[a [c d] b] @[1 (tuple 4 3) 2]] (+ a b c d)) 10) "double destructured let")

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

# Dynamic defs

(def staticdef1 0)
(defn staticdef1-inc [] (+ 1 staticdef1))
(assert (= 1 (staticdef1-inc)) "before redefinition without :redef")
(def staticdef1 1)
(assert (= 1 (staticdef1-inc)) "after redefinition without :redef")
(setdyn :redef true)
(def dynamicdef2 0)
(defn dynamicdef2-inc [] (+ 1 dynamicdef2))
(assert (= 1 (dynamicdef2-inc)) "before redefinition with dyn :redef")
(def dynamicdef2 1)
(assert (= 2 (dynamicdef2-inc)) "after redefinition with dyn :redef")
(setdyn :redef nil)

# Denormal tables and structs

(assert (= (length {1 2 nil 3}) 1) "nil key struct literal")
(assert (= (length @{1 2 nil 3}) 1) "nil key table literal")
(assert (= (length (struct 1 2 nil 3)) 1) "nil key struct ctor")
(assert (= (length (table 1 2 nil 3)) 1) "nil key table ctor")

(assert (= (length (struct (/ 0 0) 2 1 3)) 1) "nan key struct ctor")
(assert (= (length (table (/ 0 0) 2 1 3)) 1) "nan key table ctor")
(assert (= (length {1 2 nil 3}) 1) "nan key struct literal")
(assert (= (length @{1 2 nil 3}) 1) "nan key table literal")

(assert (= (length (struct 2 1 3 nil)) 1) "nil value struct ctor")
(assert (= (length (table 2 1 3 nil)) 1) "nil value table ctor")
(assert (= (length {1 2 3 nil}) 1) "nil value struct literal")
(assert (= (length @{1 2 3 nil}) 1) "nil value table literal")

# Regression Test
(assert (= 1 (((compile '(fn [] 1) @{})))) "regression test")

# Regression Test #137
(def [a b c] (range 10))
(assert (= a 0) "regression #137 (1)")
(assert (= b 1) "regression #137 (2)")
(assert (= c 2) "regression #137 (3)")

(var [x y z] (range 10))
(assert (= x 0) "regression #137 (4)")
(assert (= y 1) "regression #137 (5)")
(assert (= z 2) "regression #137 (6)")

(assert (= true ;(map truthy? [0 "" true @{} {} [] '()])) "truthy values")
(assert (= false ;(map truthy? [nil false])) "non-truthy values")

# Struct and Table duplicate elements
(assert (= {:a 3 :b 2} {:a 1 :b 2 :a 3}) "struct literal duplicate keys")
(assert (= {:a 3 :b 2} (struct :a 1 :b 2 :a 3)) "struct constructor duplicate keys")
(assert (deep= @{:a 3 :b 2} @{:a 1 :b 2 :a 3}) "table literal duplicate keys")
(assert (deep= @{:a 3 :b 2} (table :a 1 :b 2 :a 3)) "table constructor duplicate keys")

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
(assert (compare<= 1.0 2.0 3.0 3.0 4.0 5.0 6.0) "compare less than or equal to reals")
(assert (compare>= 6 5 4 4 3 2 1) "compare greater than or equal to integers")
(assert (compare>= 6.0 5.0 4.0 4.0 3.0 2.0 1.0) "compare greater than or equal to reals")
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
  (assert (= 1 (compare (table/setproto @{:v 4} mynum) n3)) "compare object to object")
  (assert (compare< 2 n3 4) "compare< poly")
  (assert (compare> 4 n3 2) "compare> poly")
  (assert (compare<= 2 3 n3 4) "compare<= poly")
  (assert (compare= 3 n3 (table/setproto @{:v 3} mynum)) "compare= poly")
  (assert (deep= (sorted @[4 5 n3 2] compare<) @[2 n3 4 5]) "polymorphic sort"))

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
       [(int/s64 MAX_INT_IN_DBL_STRING) (scan-number MAX_INT_IN_DBL_STRING) 0]
       [(int/u64 MAX_INT_IN_DBL_STRING) (scan-number MAX_INT_IN_DBL_STRING) 0]
       [(+ 1 (int/u64 MAX_INT_IN_DBL_STRING)) (scan-number MAX_INT_IN_DBL_STRING) 1]
       [(int/s64 0) INF -1] [(int/u64 0) INF -1]
       [MINUS_INF (int/u64 0) -1] [MINUS_INF (int/s64 0) -1]
       [(int/s64 1) NAN 0] [NAN (int/u64 1) 0]]]
  (each [x y c] compare-poly-tests
    (assert (= c (compare x y)) (string/format "compare polymorphic %q %q %d" x y c))))

(assert (= nil (any? [])) "any? 1")
(assert (= nil (any? [false nil])) "any? 2")
(assert (= nil (any? [nil false])) "any? 3")
(assert (= 1 (any? [1])) "any? 4")
(assert (nan? (any? [nil math/nan nil])) "any? 5")
(assert (= true (any? [nil nil false nil nil true nil nil nil nil false :a nil])) "any? 6")

(end-suite)

(start-suite 1)

(assert (= 400 (math/sqrt 160000)) "sqrt(160000)=400")

(def test-struct {'def 1 'bork 2 'sam 3 'a 'b 'het @[1 2 3 4 5]})
(assert (= (get test-struct 'def) 1) "struct get")
(assert (= (get test-struct 'bork) 2) "struct get")
(assert (= (get test-struct 'sam) 3) "struct get")
(assert (= (get test-struct 'a) 'b) "struct get")
(assert (= :array (type (get test-struct 'het))) "struct get")

(defn myfun [x]
  (var a 10)
  (set a (do
         (def y x)
         (if x 8 9))))

(assert (= (myfun true) 8) "check do form regression")
(assert (= (myfun false) 9) "check do form regression")

(defn assert-many [f n e]
 (var good true)
 (loop [i :range [0 n]]
  (if (not (f))
   (set good false)))
 (assert good e))

(assert-many (fn [] (>= 1 (math/random) 0)) 200 "(random) between 0 and 1")

## Table prototypes

(def roottab @{
 :parentprop 123
})

(def childtab @{
 :childprop 456
})

(table/setproto childtab roottab)

(assert (= 123 (get roottab :parentprop)) "table get 1")
(assert (= 123 (get childtab :parentprop)) "table get proto")
(assert (= nil (get roottab :childprop)) "table get 2")
(assert (= 456 (get childtab :childprop)) "proto no effect")

# Long strings

(assert (= "hello, world" `hello, world`) "simple long string")
(assert (= "hello, \"world\"" `hello, "world"`) "long string with embedded quotes")
(assert (= "hello, \\\\\\ \"world\"" `hello, \\\ "world"`)
        "long string with embedded quotes and backslashes")

# More fiber semantics

(var myvar 0)
(defn fiberstuff [&]
  (++ myvar)
  (def f (fiber/new (fn [&] (++ myvar) (debug) (++ myvar))))
  (resume f)
  (++ myvar))

(def myfiber (fiber/new fiberstuff :dey))

(assert (= myvar 0) "fiber creation does not call fiber function")
(resume myfiber)
(assert (= myvar 2) "fiber debug statement breaks at proper point")
(assert (= (fiber/status myfiber) :debug) "fiber enters debug state")
(resume myfiber)
(assert (= myvar 4) "fiber resumes properly from debug state")
(assert (= (fiber/status myfiber) :dead) "fiber properly dies from debug state")

# Test max triangle program

# Find the maximum path from the top (root)
# of the triangle to the leaves of the triangle.

(defn myfold [xs ys]
  (let [xs1 [;xs 0]
        xs2 [0 ;xs]
        m1 (map + xs1 ys)
        m2 (map + xs2 ys)]
    (map max m1 m2)))

(defn maxpath [t]
 (extreme > (reduce myfold () t)))

# Test it
# Maximum path is 3 -> 10 -> 3 -> 9 for a total of 25

(def triangle '[
 [3]
 [7 10]
 [4 3 7]
 [8 9 1 3]
])

(assert (= (maxpath triangle) 25) `max triangle`)

(assert (= (string/join @["one" "two" "three"]) "onetwothree") "string/join 1 argument")
(assert (= (string/join @["one" "two" "three"] ", ") "one, two, three") "string/join 2 arguments")
(assert (= (string/join @[] ", ") "") "string/join empty array")

(assert (= (string/find "123" "abc123def") 3) "string/find positive")
(assert (= (string/find "1234" "abc123def") nil) "string/find negative")

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

# Marshal

(def um-lookup (env-lookup (fiber/getenv (fiber/current))))
(def m-lookup (invert um-lookup))

(defn testmarsh [x msg]
  (def marshx (marshal x m-lookup))
  (def out (marshal (unmarshal marshx um-lookup) m-lookup))
  (assert (= (string marshx) (string out)) msg))

(testmarsh nil "marshal nil")
(testmarsh false "marshal false")
(testmarsh true "marshal true")
(testmarsh 1 "marshal small integers")
(testmarsh -1 "marshal integers (-1)")
(testmarsh 199 "marshal small integers (199)")
(testmarsh 5000 "marshal medium integers (5000)")
(testmarsh -5000 "marshal small integers (-5000)")
(testmarsh 10000 "marshal large integers (10000)")
(testmarsh -10000 "marshal large integers (-10000)")
(testmarsh 1.0 "marshal double")
(testmarsh "doctordolittle" "marshal string")
(testmarsh :chickenshwarma "marshal symbol")
(testmarsh @"oldmcdonald" "marshal buffer")
(testmarsh @[1 2 3 4 5] "marshal array")
(testmarsh [tuple 1 2 3 4 5] "marshal tuple")
(testmarsh @{1 2 3 4}  "marshal table")
(testmarsh {1 2 3 4}  "marshal struct")
(testmarsh (fn [x] x) "marshal function 0")
(testmarsh (fn name [x] x) "marshal function 1")
(testmarsh (fn [x] (+ 10 x 2)) "marshal function 2")
(testmarsh (fn thing [x] (+ 11 x x 30)) "marshal function 3")
(testmarsh map "marshal function 4")
(testmarsh reduce "marshal function 5")
(testmarsh (fiber/new (fn [] (yield 1) 2)) "marshal simple fiber 1")
(testmarsh (fiber/new (fn [&] (yield 1) 2)) "marshal simple fiber 2")

(def strct {:a @[nil]})
(put (strct :a) 0 strct)
(testmarsh strct "cyclic struct")

# Large functions
(def manydefs (seq [i :range [0 300]] (tuple 'def (gensym) (string "value_" i))))
(array/push manydefs (tuple * 10000 3 5 7 9))
(def f (compile ['do ;manydefs] (fiber/getenv (fiber/current))))
(assert (= (f) (* 10000 3 5 7 9)) "long function compilation")

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

# Testing the loop and seq macros
(def xs (apply tuple (seq [x :range [0 10] :when (even? x)] (tuple (/ x 2) x))))
(assert (= xs '((0 0) (1 2) (2 4) (3 6) (4 8))) "seq macro 1")

(def xs (apply tuple (seq [x :down [8 -2] :when (even? x)] (tuple (/ x 2) x))))
(assert (= xs '((4 8) (3 6) (2 4) (1 2) (0 0))) "seq macro 2")

(def xs (catseq [x :range [0 3]] [x x]))
(assert (deep= xs @[0 0 1 1 2 2]) "catseq")

# :range-to and :down-to
(assert (deep= (seq [x :range-to [0 10]] x) (seq [x :range [0 11]] x)) "loop :range-to")
(assert (deep= (seq [x :down-to [10 0]] x) (seq [x :down [10 -1]] x)) "loop :down-to")

# Some testing for not=
(assert (not= 1 1 0) "not= 1")
(assert (not= 0 1 1) "not= 2")

# Closure in while loop
(def closures (seq [i :range [0 5]] (fn [] i)))
(assert (= 0 ((get closures 0))) "closure in loop 0")
(assert (= 1 ((get closures 1))) "closure in loop 1")
(assert (= 2 ((get closures 2))) "closure in loop 2")
(assert (= 3 ((get closures 3))) "closure in loop 3")
(assert (= 4 ((get closures 4))) "closure in loop 4")

# More numerical tests
(assert (= 1 1.0) "numerical equal 1")
(assert (= 0 0.0) "numerical equal 2")
(assert (= 0 -0.0) "numerical equal 3")
(assert (= 2_147_483_647 2_147_483_647.0) "numerical equal 4")
(assert (= -2_147_483_648 -2_147_483_648.0) "numerical equal 5")

# Array tests

(defn array=
  "Check if two arrays are equal in an element by element comparison"
  [a b]
  (if (and (array? a) (array? b))
    (= (apply tuple a) (apply tuple b))))
(assert (= (apply tuple @[1 2 3 4 5]) (tuple 1 2 3 4 5)) "array to tuple")
(def arr (array))
(array/push arr :hello)
(array/push arr :world)
(assert (array= arr @[:hello :world]) "array comparison")
(assert (array= @[1 2 3 4 5] @[1 2 3 4 5]) "array comparison 2")
(assert (array= @[:one :two :three :four :five] @[:one :two :three :four :five]) "array comparison 3")
(assert (array= (array/slice @[1 2 3] 0 2) @[1 2]) "array/slice 1")
(assert (array= (array/slice @[0 7 3 9 1 4] 2 -2) @[3 9 1]) "array/slice 2")

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
(assert (deep= (map + [1 2 3] [10 20 30] [100 200 300] [1000 2000 3000]) @[1111 2222 3333]))
(assert (deep= (map + [1 2 3] [10 20 30] [100 200 300] [1000 2000 3000] [10000 20000 30000]) @[11111 22222 33333]))
(assert (deep= (map + [1 2 3] [10 20 30] [100 200 300] [1000 2000 3000] [10000 20000 30000] [100000 200000 300000]) @[111111 222222 333333]))

# Mapping uses the shortest sequence
(assert (deep= (map + [1 2 3 4] [10 20 30]) @[11 22 33]))
(assert (deep= (map + [1 2 3 4] [10 20 30] [100 200]) @[111 222]))
(assert (deep= (map + [1 2 3 4] [10 20 30] [100 200] [1000]) @[1111]))
(assert (deep= (map + [1 2 3 4] [10 20 30] [100 200] [1000] []) @[]))

# Variadic arguments to map-like functions
(assert (deep= (mapcat tuple [1 2 3 4] [5 6 7 8]) @[1 5 2 6 3 7 4 8]))
(assert (deep= (keep |(if (> $1 0) (/ $0 $1)) [1 2 3 4 5] [1 2 1 0 1]) @[1 1 3 5]))

(assert (= (count = [1 3 2 4 3 5 4 2 1] [1 2 3 4 5 4 3 2 1]) 4))

(assert (= (some not= (range 5) (range 5)) nil))
(assert (= (some = [1 2 3 4 5] [5 4 3 2 1]) true))

(assert (= (all = (range 5) (range 5)) true))
(assert (= (all not= [1 2 3 4 5] [5 4 3 2 1]) false))

(assert (= false (deep-not= [1] [1])) "issue #1149")

# Sort function
(assert (deep=
          (range 99)
          (sort (mapcat (fn [[x y z]] [z y x]) (partition 3 (range 99))))) "sort 5")
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

(def yielder
  (coro
    (defer (yield :end)
      (repeat 5 (yield :item)))))
(def items (seq [x :in yielder] x))
(assert (deep= @[:item :item :item :item :item :end] items) "yield within nested fibers")

(end-suite)
