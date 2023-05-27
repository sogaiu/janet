# Copyright (c) 2023 Calvin Rose
# Copyright (c) 2023 Calvin Rose & contributors
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
(start-suite 2)

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

# Looping idea
(def xs
  (seq [x :in [-1 0 1] y :in [-1 0 1] :when (not= x y 0)] (tuple x y)))
(def txs (apply tuple xs))

(assert (= txs [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]) "nested seq")

# Generators
(def gen (generate [x :range [0 100] :when (pos? (% x 4))] x))
(var gencount 0)
(loop [x :in gen]
  (++ gencount)
  (assert (pos? (% x 4)) "generate in loop"))
(assert (= gencount 75) "generate loop count")

# Check x:digits: works as symbol and not a hex number
(def x1 100)
(assert (= x1 100) "x1 as symbol")
(def X1 100)
(assert (= X1 100) "X1 as symbol")

# String functions
(assert (= 3 (string/find "abc" "   abcdefghijklmnop")) "string/find 1")
(assert (= 0 (string/find "A" "A")) "string/find 2")
(assert (string/has-prefix? "" "foo") "string/has-prefix? 1")
(assert (string/has-prefix? "fo" "foo") "string/has-prefix? 2")
(assert (not (string/has-prefix? "o" "foo")) "string/has-prefix? 3")
(assert (string/has-suffix? "" "foo") "string/has-suffix? 1")
(assert (string/has-suffix? "oo" "foo") "string/has-suffix? 2")
(assert (not (string/has-suffix? "f" "foo")) "string/has-suffix? 3")
(assert (= (string/replace "X" "." "XXX...XXX...XXX")  ".XX...XXX...XXX") "string/replace 1")
(assert (= (string/replace-all "X" "." "XXX...XXX...XXX") "...............") "string/replace-all 1")
(assert (= (string/replace-all "XX" "." "XXX...XXX...XXX") ".X....X....X") "string/replace-all 2")
(assert (= (string/replace "xx" string/ascii-upper "xxyxyxyxxxy") "XXyxyxyxxxy") "string/replace function")
(assert (= (string/replace-all "xx" string/ascii-upper "xxyxyxyxxxy") "XXyxyxyXXxy") "string/replace-all function")
(assert (= (string/replace "x" 12 "xyx") "12yx") "string/replace stringable")
(assert (= (string/replace-all "x" 12 "xyx") "12y12") "string/replace-all stringable")
(assert (= (string/ascii-lower "ABCabc&^%!@:;.") "abcabc&^%!@:;.") "string/ascii-lower")
(assert (= (string/ascii-upper "ABCabc&^%!@:;.") "ABCABC&^%!@:;.") "string/ascii-lower")
(assert (= (string/reverse "") "") "string/reverse 1")
(assert (= (string/reverse "a") "a") "string/reverse 2")
(assert (= (string/reverse "abc") "cba") "string/reverse 3")
(assert (= (string/reverse "abcd") "dcba") "string/reverse 4")
(assert (= (string/join @["one" "two" "three"] ",") "one,two,three") "string/join 1")
(assert (= (string/join @["one" "two" "three"] ", ") "one, two, three") "string/join 2")
(assert (= (string/join @["one" "two" "three"]) "onetwothree") "string/join 3")
(assert (= (string/join @[] "hi") "") "string/join 4")
(assert (= (string/trim " abcd ") "abcd") "string/trim 1")
(assert (= (string/trim "abcd \t\t\r\f") "abcd") "string/trim 2")
(assert (= (string/trim "\n\n\t abcd") "abcd") "string/trim 3")
(assert (= (string/trim "") "") "string/trim 4")
(assert (= (string/triml " abcd ") "abcd ") "string/triml 1")
(assert (= (string/triml "\tabcd \t\t\r\f") "abcd \t\t\r\f") "string/triml 2")
(assert (= (string/triml "abcd ") "abcd ") "string/triml 3")
(assert (= (string/trimr " abcd ") " abcd") "string/trimr 1")
(assert (= (string/trimr "\tabcd \t\t\r\f") "\tabcd") "string/trimr 2")
(assert (= (string/trimr " abcd") " abcd") "string/trimr 3")
(assert (deep= (string/split "," "one,two,three") @["one" "two" "three"]) "string/split 1")
(assert (deep= (string/split "," "onetwothree") @["onetwothree"]) "string/split 2")
(assert (deep= (string/find-all "e" "onetwothree") @[2 9 10]) "string/find-all 1")
(assert (deep= (string/find-all "," "onetwothree") @[]) "string/find-all 2")

(assert-error "string/find error 1" (string/find "" "abcd"))
(assert-error "string/split error 1" (string/split "" "abcd"))
(assert-error "string/replace error 1" (string/replace "" "." "abcd"))
(assert-error "string/replace-all error 1" (string/replace-all "" "." "abcdabcd"))
(assert-error "string/find-all error 1" (string/find-all "" "abcd"))

# Check if abstract test works
(assert (abstract? stdout) "abstract? stdout")
(assert (abstract? stdin) "abstract? stdin")
(assert (abstract? stderr) "abstract? stderr")
(assert (not (abstract? nil)) "not abstract? nil")
(assert (not (abstract? 1)) "not abstract? 1")
(assert (not (abstract? 3)) "not abstract? 3")
(assert (not (abstract? 5)) "not abstract? 5")

(end-suite)

(start-suite 3)

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

# Edge case should cause old compilers to fail due to
# if statement optimization
(var var-a 1)
(var var-b (if false 2 (string "hello")))

(assert (= var-b "hello") "regression 1")

# Scan number

(assert (= 1 (scan-number "1")) "scan-number 1")
(assert (= -1 (scan-number "-1")) "scan-number -1")
(assert (= 1.3e4 (scan-number "1.3e4")) "scan-number 1.3e4")

# Some macros

(assert (= 2 (if-not 1 3 2)) "if-not 1")
(assert (= 3 (if-not false 3)) "if-not 2")
(assert (= 3 (if-not nil 3 2)) "if-not 3")
(assert (= nil (if-not true 3)) "if-not 4")

(assert (= 4 (unless false (+ 1 2 3) 4)) "unless")

(def res @{})
(loop [[k v] :pairs @{1 2 3 4 5 6}]
  (put res k v))
(assert (and
          (= (get res 1) 2)
          (= (get res 3) 4)
          (= (get res 5) 6)) "loop :pairs")

# Another regression test - no segfaults
(defn afn [x] x)
(var afn-var afn)
(var identity-var identity)
(var map-var map)
(var not-var not)
(assert (= 1 (try (afn-var) ([err] 1))) "bad arity 1")
(assert (= 4 (try ((fn [x y] (+ x y)) 1) ([_] 4))) "bad arity 2")
(assert (= 1 (try (identity-var) ([err] 1))) "bad arity 3")
(assert (= 1 (try (map-var) ([err] 1))) "bad arity 4")
(assert (= 1 (try (not-var) ([err] 1))) "bad arity 5")

# Assembly test
# Fibonacci sequence, implemented with naive recursion.
(def fibasm (asm '{
  :arity 1
  :bytecode [
    (ltim 1 0 0x2)      # $1 = $0 < 2
    (jmpif 1 :done)     # if ($1) goto :done
    (lds 1)             # $1 = self
    (addim 0 0 -0x1)    # $0 = $0 - 1
    (push 0)            # push($0), push argument for next function call
    (call 2 1)          # $2 = call($1)
    (addim 0 0 -0x1)    # $0 = $0 - 1
    (push 0)            # push($0)
    (call 0 1)          # $0 = call($1)
    (add 0 0 2)        # $0 = $0 + $2 (integers)
    :done
    (ret 0)             # return $0
  ]
}))

(assert (= 0 (fibasm 0)) "fibasm 1")
(assert (= 1 (fibasm 1)) "fibasm 2")
(assert (= 55 (fibasm 10)) "fibasm 3")
(assert (= 6765 (fibasm 20)) "fibasm 4")

# Calling non functions

(assert (= 1 ({:ok 1} :ok)) "calling struct")
(assert (= 2 (@{:ok 2} :ok)) "calling table")
(assert (= :bad (try ((identity @{:ok 2}) :ok :no) ([err] :bad))) "calling table too many arguments")
(assert (= :bad (try ((identity :ok) @{:ok 2} :no) ([err] :bad))) "calling keyword too many arguments")
(assert (= :oops (try ((+ 2 -1) 1) ([err] :oops))) "calling number fails")

# Method test

(def Dog @{:bark (fn bark [self what] (string (self :name) " says " what "!"))})
(defn make-dog
  [name]
  (table/setproto @{:name name} Dog))

(assert (= "fido" ((make-dog "fido") :name)) "oo 1")
(def spot (make-dog "spot"))
(assert (= "spot says hi!" (:bark spot "hi")) "oo 2")

# Negative tests

(assert-error "+ check types" (+ 1 ()))
(assert-error "- check types" (- 1 ()))
(assert-error "* check types" (* 1 ()))
(assert-error "/ check types" (/ 1 ()))
(assert-error "band check types" (band 1 ()))
(assert-error "bor check types" (bor 1 ()))
(assert-error "bxor check types" (bxor 1 ()))
(assert-error "bnot check types" (bnot ()))

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
(assert (= (string buf1) (string/repeat "1234567890" 16)) "buffer blit against self")

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
(assert (= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" (string b4)) "buffer/push-buffer 1")
(def b5 @"123")
(buffer/push-string b5 "456" @"789")
(assert (= "123456789" (string b5)) "buffer/push-buffer 2")

# Check for bugs with printing self with buffer/format

(def buftemp @"abcd")
(assert (= (string (buffer/format buftemp "---%p---" buftemp)) `abcd---@"abcd"---`) "buffer/format on self 1")
(def buftemp @"abcd")
(assert (= (string (buffer/format buftemp "---%p %p---" buftemp buftemp)) `abcd---@"abcd" @"abcd"---`) "buffer/format on self 2")

# Peg

(defn check-match
  [pat text should-match]
  (def result (peg/match pat text))
  (assert (= (not should-match) (not result)) (string "check-match " text)))

(defn check-deep
  [pat text what]
  (def result (peg/match pat text))
  (assert (deep= result what) (string "check-deep " text)))

# Just numbers

(check-match '(* 4 -1) "abcd" true)
(check-match '(* 4 -1) "abc" false)
(check-match '(* 4 -1) "abcde" false)

# Simple pattern

(check-match '(* (some (range "az" "AZ")) -1) "hello" true)
(check-match '(* (some (range "az" "AZ")) -1) "hello world" false)
(check-match '(* (some (range "az" "AZ")) -1) "1he11o" false)
(check-match '(* (some (range "az" "AZ")) -1) "" false)

# Pre compile

(def pegleg (peg/compile '{:item "abc" :main (* :item "," :item -1)}))

(peg/match pegleg "abc,abc")

# Bad Grammars

(assert-error "peg/compile error 1" (peg/compile nil))
(assert-error "peg/compile error 2" (peg/compile @{}))
(assert-error "peg/compile error 3" (peg/compile '{:a "abc" :b "def"}))
(assert-error "peg/compile error 4" (peg/compile '(blarg "abc")))
(assert-error "peg/compile error 5" (peg/compile '(1 2 3)))

# IP address

(def ip-address
  '{:d (range "09")
    :0-4 (range "04")
    :0-5 (range "05")
    :byte (+
            (* "25" :0-5)
            (* "2" :0-4 :d)
            (* "1" :d :d)
            (between 1 2 :d))
    :main (* :byte "." :byte "." :byte "." :byte)})

(check-match ip-address "10.240.250.250" true)
(check-match ip-address "0.0.0.0" true)
(check-match ip-address "1.2.3.4" true)
(check-match ip-address "256.2.3.4" false)
(check-match ip-address "256.2.3.2514" false)

# Substitution test with peg

(file/flush stderr)
(file/flush stdout)

(def grammar '(accumulate (any (+ (/ "dog" "purple panda") (<- 1)))))
(defn try-grammar [text]
  (assert (= (string/replace-all "dog" "purple panda" text) (0 (peg/match grammar text))) text))

(try-grammar "i have a dog called doug the dog. he is good.")
(try-grammar "i have a dog called doug the dog. he is a good boy.")
(try-grammar "i have a dog called doug the do")
(try-grammar "i have a dog called doug the dog")
(try-grammar "i have a dog called doug the dogg")
(try-grammar "i have a dog called doug the doggg")
(try-grammar "i have a dog called doug the dogggg")

# Peg CSV test

(def csv
  '{:field (+
            (* `"` (% (any (+ (<- (if-not `"` 1)) (* (constant `"`) `""`)))) `"`)
            (<- (any (if-not (set ",\n") 1))))
    :main (* :field (any (* "," :field)) (+ "\n" -1))})

(defn check-csv
  [str res]
  (check-deep csv str res))

(check-csv "1,2,3" @["1" "2" "3"])
(check-csv "1,\"2\",3" @["1" "2" "3"])
(check-csv ``1,"1""",3`` @["1" "1\"" "3"])

# Nested Captures

(def grmr '(capture (* (capture "a") (capture 1) (capture "c"))))
(check-deep grmr "abc" @["a" "b" "c" "abc"])
(check-deep grmr "acc" @["a" "c" "c" "acc"])

# Functions in grammar

(def grmr-triple ~(% (any (/ (<- 1) ,(fn [x] (string x x x))))))
(check-deep grmr-triple "abc" @["aaabbbccc"])
(check-deep grmr-triple "" @[""])
(check-deep grmr-triple " " @["   "])

(def counter ~(/ (group (any (<- 1))) ,length))
(check-deep counter "abcdefg" @[7])

# Capture Backtracking

(check-deep '(+ (* (capture "c") "d") "ce") "ce" @[])

# Matchtime capture

(def scanner (peg/compile ~(cmt (capture (some 1)) ,scan-number)))

(check-deep scanner "123" @[123])
(check-deep scanner "0x86" @[0x86])
(check-deep scanner "-1.3e-7" @[-1.3e-7])
(check-deep scanner "123A" nil)

# Recursive grammars

(def g '{:main (+ (* "a" :main "b") "c")})

(check-match g "c" true)
(check-match g "acb" true)
(check-match g "aacbb" true)
(check-match g "aadbb" false)

# Back reference

(def wrapped-string
  ~{:pad (any "=")
    :open (* "[" (<- :pad :n) "[")
    :close (* "]" (cmt (* (-> :n) (<- :pad)) ,=) "]")
    :main (* :open (any (if-not :close 1)) :close -1)})

(check-match wrapped-string "[[]]" true)
(check-match wrapped-string "[==[a]==]" true)
(check-match wrapped-string "[==[]===]" false)
(check-match wrapped-string "[[blark]]" true)
(check-match wrapped-string "[[bl[ark]]" true)
(check-match wrapped-string "[[bl]rk]]" true)
(check-match wrapped-string "[[bl]rk]] " false)
(check-match wrapped-string "[=[bl]]rk]=] " false)
(check-match wrapped-string "[=[bl]==]rk]=] " false)
(check-match wrapped-string "[===[]==]===]" true)

(def janet-longstring
  ~{:delim (some "`")
    :open (capture :delim :n)
    :close (cmt (* (not (> -1 "`")) (-> :n) (<- (backmatch :n))) ,=)
    :main (* :open (any (if-not :close 1)) :close -1)})

(check-match janet-longstring "`john" false)
(check-match janet-longstring "abc" false)
(check-match janet-longstring "` `" true)
(check-match janet-longstring "`  `" true)
(check-match janet-longstring "``  ``" true)
(check-match janet-longstring "``` `` ```" true)
(check-match janet-longstring "``  ```" false)
(check-match janet-longstring "`a``b`" false)

# Line and column capture

(def line-col (peg/compile '(any (* (line) (column) 1))))
(check-deep line-col "abcd" @[1 1 1 2 1 3 1 4])
(check-deep line-col "" @[])
(check-deep line-col "abcd\n" @[1 1 1 2 1 3 1 4 1 5])
(check-deep line-col "abcd\nz" @[1 1 1 2 1 3 1 4 1 5 2 1])

# Backmatch

(def backmatcher-1 '(* (capture (any "x") :1) "y" (backmatch :1) -1))

(check-match backmatcher-1 "y" true)
(check-match backmatcher-1 "xyx" true)
(check-match backmatcher-1 "xxxxxxxyxxxxxxx" true)
(check-match backmatcher-1 "xyxx" false)
(check-match backmatcher-1 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy" false)
(check-match backmatcher-1 (string (string/repeat "x" 10000) "y") false)
(check-match backmatcher-1 (string (string/repeat "x" 10000) "y" (string/repeat "x" 10000)) true)

(def backmatcher-2 '(* '(any "x") "y" (backmatch) -1))

(check-match backmatcher-2 "y" true)
(check-match backmatcher-2 "xyx" true)
(check-match backmatcher-2 "xxxxxxxyxxxxxxx" true)
(check-match backmatcher-2 "xyxx" false)
(check-match backmatcher-2 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy" false)
(check-match backmatcher-2 (string (string/repeat "x" 10000) "y") false)
(check-match backmatcher-2 (string (string/repeat "x" 10000) "y" (string/repeat "x" 10000)) true)

(def longstring-2 '(* '(some "`") (some (if-not (backmatch) 1)) (backmatch) -1))

(check-match longstring-2 "`john" false)
(check-match longstring-2 "abc" false)
(check-match longstring-2 "` `" true)
(check-match longstring-2 "`  `" true)
(check-match longstring-2 "``  ``" true)
(check-match longstring-2 "``` `` ```" true)
(check-match longstring-2 "``  ```" false)

# Optional

(check-match '(* (opt "hi") -1) "" true)
(check-match '(* (opt "hi") -1) "hi" true)
(check-match '(* (opt "hi") -1) "no" false)
(check-match '(* (? "hi") -1) "" true)
(check-match '(* (? "hi") -1) "hi" true)
(check-match '(* (? "hi") -1) "no" false)

# Drop

(check-deep '(drop '"hello") "hello" @[])
(check-deep '(drop "hello") "hello" @[])

# Regression #24

(def t (put @{} :hi 1))
(assert (deep= t @{:hi 1}) "regression #24")

# Peg swallowing errors
(assert (try (peg/match ~(/ '1 ,(fn [x] (nil x))) "x") ([err] err))
        "errors should not be swallowed")
(assert (try ((fn [x] (nil x))) ([err] err))
        "errors should not be swallowed 2")

# Tuple types

(assert (= (tuple/type '(1 2 3)) :parens) "normal tuple")
(assert (= (tuple/type [1 2 3]) :parens) "normal tuple 1")
(assert (= (tuple/type '[1 2 3]) :brackets) "bracketed tuple 2")
(assert (= (tuple/type (-> '(1 2 3) marshal unmarshal)) :parens) "normal tuple marshalled/unmarshalled")
(assert (= (tuple/type (-> '[1 2 3] marshal unmarshal)) :brackets) "normal tuple marshalled/unmarshalled")

# Check for bad memoization (+ :a) should mean different things in different contexts.
(def redef-a
  ~{:a "abc"
    :c (+ :a)
    :main (* :c {:a "def" :main (+ :a)} -1)})

(check-match redef-a "abcdef" true)
(check-match redef-a "abcabc" false)
(check-match redef-a "defdef" false)

(def redef-b
  ~{:pork {:pork "beef" :main (+ -1 (* 1 :pork))}
    :main :pork})

(check-match redef-b "abeef" true)
(check-match redef-b "aabeef" false)
(check-match redef-b "aaaaaa" false)

# Integer parsing

(check-deep '(int 1) "a" @[(chr "a")])
(check-deep '(uint 1) "a" @[(chr "a")])
(check-deep '(int-be 1) "a" @[(chr "a")])
(check-deep '(uint-be 1) "a" @[(chr "a")])
(check-deep '(int 1) "\xFF" @[-1])
(check-deep '(uint 1) "\xFF" @[255])
(check-deep '(int-be 1) "\xFF" @[-1])
(check-deep '(uint-be 1) "\xFF" @[255])
(check-deep '(int 2) "\xFF\x7f" @[0x7fff])
(check-deep '(int-be 2) "\x7f\xff" @[0x7fff])
(check-deep '(uint 2) "\xff\x7f" @[0x7fff])
(check-deep '(uint-be 2) "\x7f\xff" @[0x7fff])
(check-deep '(uint-be 2) "\x7f\xff" @[0x7fff])
(check-deep '(uint 8) "\xff\x7f\x00\x00\x00\x00\x00\x00" @[(int/u64 0x7fff)])
(check-deep '(int 8) "\xff\x7f\x00\x00\x00\x00\x00\x00" @[(int/s64 0x7fff)])
(check-deep '(uint 7) "\xff\x7f\x00\x00\x00\x00\x00" @[(int/u64 0x7fff)])
(check-deep '(int 7) "\xff\x7f\x00\x00\x00\x00\x00" @[(int/s64 0x7fff)])

(check-deep '(* (int 2) -1) "123" nil)

# to/thru bug
(check-deep '(to -1) "aaaa" @[])
(check-deep '(thru -1) "aaaa" @[])
(check-deep ''(to -1) "aaaa" @["aaaa"])
(check-deep ''(thru -1) "aaaa" @["aaaa"])
(check-deep '(to "b") "aaaa" nil)
(check-deep '(thru "b") "aaaa" nil)

# unref
(def grammar
  (peg/compile
    ~{:main (* :tagged -1)
      :tagged (unref (replace (* :open-tag :value :close-tag) ,struct))
      :open-tag (* (constant :tag) "<" (capture :w+ :tag-name) ">")
      :value (* (constant :value) (group (any (+ :tagged :untagged))))
      :close-tag (* "</" (backmatch :tag-name) ">")
      :untagged (capture (any (if-not "<" 1)))}))
(check-deep grammar "<p><em>foobar</em></p>" @[{:tag "p" :value @[{:tag "em" :value @["foobar"]}]}])
(check-deep grammar "<p>foobar</p>" @[{:tag "p" :value @["foobar"]}])

(end-suite)
(start-suite 4)
# some tests for string/format and buffer/format

(assert (= (string (buffer/format @"" "pi = %6.3f" math/pi)) "pi =  3.142") "%6.3f")
(assert (= (string (buffer/format @"" "pi = %+6.3f" math/pi)) "pi = +3.142") "%6.3f")
(assert (= (string (buffer/format @"" "pi = %40.20g" math/pi)) "pi =                     3.141592653589793116") "%6.3f")

(assert (= (string (buffer/format @"" "🐼 = %6.3f" math/pi)) "🐼 =  3.142") "UTF-8")
(assert (= (string (buffer/format @"" "π = %.8g" math/pi)) "π = 3.1415927") "π")
(assert (= (string (buffer/format @"" "\xCF\x80 = %.8g" math/pi)) "\xCF\x80 = 3.1415927") "\xCF\x80")

(assert (= (string/format "pi = %6.3f" math/pi) "pi =  3.142") "%6.3f")
(assert (= (string/format "pi = %+6.3f" math/pi) "pi = +3.142") "%6.3f")
(assert (= (string/format "pi = %40.20g" math/pi) "pi =                     3.141592653589793116") "%6.3f")

(assert (= (string/format "🐼 = %6.3f" math/pi) "🐼 =  3.142") "UTF-8")
(assert (= (string/format "π = %.8g" math/pi) "π = 3.1415927") "π")
(assert (= (string/format "\xCF\x80 = %.8g" math/pi) "\xCF\x80 = 3.1415927") "\xCF\x80")

# Range
(assert (deep= (range 10) @[0 1 2 3 4 5 6 7 8 9]) "range 1 argument")
(assert (deep= (range 5 10) @[5 6 7 8 9]) "range 2 arguments")
(assert (deep= (range 5 10 2) @[5 7 9]) "range 3 arguments")

# More marshalling code

(defn check-image
  "Run a marshaling test using the make-image and load-image functions."
  [x msg]
  (def im (make-image x))
  # (printf "\nimage-hash: %d" (-> im string hash))
  (assert-no-error msg (load-image im)))

(check-image (fn [] (fn [] 1)) "marshal nested functions")
(check-image (fiber/new (fn [] (fn [] 1))) "marshal nested functions in fiber")
(check-image (fiber/new (fn [] (fiber/new (fn [] 1)))) "marshal nested fibers")

(def issue-53-x 
  (fiber/new 
    (fn [] 
      (var y (fiber/new (fn [] (print "1") (yield) (print "2")))))))

(check-image issue-53-x "issue 53 regression")

# Bracket tuple issue

(def do 3)
(assert (= [3 1 2 3] [do 1 2 3]) "bracket tuples are never special forms")
(assert (= ~(,defn 1 2 3) [defn 1 2 3]) "bracket tuples are never macros")
(assert (= ~(,+ 1 2 3) [+ 1 2 3]) "bracket tuples are never function calls")

# Metadata

(def foo-with-tags :a-tag :bar)
(assert (get (dyn 'foo-with-tags) :a-tag) "extra keywords in def are metadata tags")

(def foo-with-meta {:baz :quux} :bar)
(assert (= :quux (get (dyn 'foo-with-meta) :baz)) "extra struct in def is metadata")

(defn foo-fn-with-meta {:baz :quux} "This is a function" [x] (identity x))
(assert (= :quux (get (dyn 'foo-fn-with-meta) :baz)) "extra struct in defn is metadata")
(assert (= "(foo-fn-with-meta x)\n\nThis is a function" (get (dyn 'foo-fn-with-meta) :doc)) "extra string in defn is docstring")

(end-suite)

(start-suite 5)

# Array remove

(assert (deep= (array/remove @[1 2 3 4 5] 2) @[1 2 4 5]) "array/remove 1")
(assert (deep= (array/remove @[1 2 3 4 5] 2 2) @[1 2 5]) "array/remove 2")
(assert (deep= (array/remove @[1 2 3 4 5] 2 200) @[1 2]) "array/remove 3")
(assert (deep= (array/remove @[1 2 3 4 5] -3 200) @[1 2 3]) "array/remove 4")

# Break

(var summation 0)
(for i 0 10
  (+= summation i)
  (if (= i 7) (break)))
(assert (= summation 28) "break 1")

(assert (= nil ((fn [] (break) 4))) "break 2")

# Break with value

# Shouldn't error out
(assert-no-error "break 3" (for i 0 10 (if (> i 8) (break i))))
(assert-no-error "break 4" ((fn [i] (if (> i 8) (break i))) 100))

# take

(assert (deep= (take 0 []) []) "take 1")
(assert (deep= (take 10 []) []) "take 2")
(assert (deep= (take 0 [1 2 3 4 5]) []) "take 3")
(assert (deep= (take 10 [1 2 3]) [1 2 3]) "take 4")
(assert (deep= (take -1 [:a :b :c]) []) "take 5")
(assert (deep= (take 3 (generate [x :in [1 2 3 4 5]] x)) @[1 2 3]) "take from fiber")
# NB: repeatedly resuming a fiber created with `generate` includes a `nil` as
# the final element. Thus a generate of 2 elements will create an array of 3.
(assert (= (length (take 4 (generate [x :in [1 2]] x))) 2) "take from short fiber")

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

# Quasiquote bracketed tuples
(assert (= (tuple/type ~[1 2 3]) (tuple/type '[1 2 3])) "quasiquote bracket tuples")

# No useless splices
(check-compile-error '((splice [1 2 3]) 0))
(check-compile-error '(if ;[1 2] 5))
(check-compile-error '(while ;[1 2 3] (print :hi)))
(check-compile-error '(def x ;[1 2 3]))
(check-compile-error '(fn [x] ;[x 1 2 3]))

# No splice propagation
(check-compile-error '(+ 1 (do ;[2 3 4]) 5))
(check-compile-error '(+ 1 (upscope ;[2 3 4]) 5))
# compiler inlines when condition is constant, ensure that optimization doesn't break
(check-compile-error '(+ 1 (if true ;[3 4])))
(check-compile-error '(+ 1 (if false nil ;[3 4])))
(start-suite 6)

# some tests for bigint

(def i64 int/s64)
(def u64 int/u64)

(assert-no-error
 "create some uint64 bigints"
 (do
   # from number
   (def a (u64 10))
   # max double we can convert to int (2^53)
   (def b (u64 0x1fffffffffffff))
   (def b (u64 (math/pow 2 53)))
   # from string
   (def c (u64 "0xffff_ffff_ffff_ffff"))
   (def c (u64 "32rvv_vv_vv_vv"))
   (def d (u64 "123456789"))))

# Conversion back to an int32
(assert (= (int/to-number (u64 0xFaFa)) 0xFaFa))
(assert (= (int/to-number (i64 0xFaFa)) 0xFaFa))
(assert (= (int/to-number (u64 9007199254740991)) 9007199254740991))
(assert (= (int/to-number (i64 9007199254740991)) 9007199254740991))
(assert (= (int/to-number (i64 -9007199254740991)) -9007199254740991))

(assert-error
  "u64 out of bounds for safe integer"
  (int/to-number (u64 "9007199254740993"))

(assert-error
  "s64 out of bounds for safe integer"
  (int/to-number (i64 "-9007199254740993"))))

(assert-error
  "int/to-number fails on non-abstract types"
  (int/to-number 1))

(assert-no-error
 "create some int64 bigints"
 (do
   # from number
   (def a (i64 -10))
   # max double we can convert to int (2^53)
   (def b (i64 0x1fffffffffffff))
   (def b (i64 (math/pow 2 53)))
   # from string
   (def c (i64 "0x7fff_ffff_ffff_ffff"))
   (def d (i64 "123456789"))))

(assert-error
 "bad initializers"
 (do
   # double to big to be converted to uint64 without truncation (2^53 + 1)
   (def b (u64 (+ 0xffff_ffff_ffff_ff 1)))
   (def b (u64 (+ (math/pow 2 53) 1)))
   # out of range 65 bits
   (def c (u64 "0x1ffffffffffffffff"))
   # just to big
   (def d (u64 "123456789123456789123456789"))))

(assert (= (:/ (u64 "0xffff_ffff_ffff_ffff") 8 2) (u64 "0xfffffffffffffff")) "bigint operations 1")
(assert (let [a (u64 0xff)] (= (:+ a a a a) (:* a 2 2))) "bigint operations 2")

(assert (= (string (i64 -123)) "-123") "i64 prints reasonably")
(assert (= (string (u64 123)) "123") "u64 prints reasonably")

(assert-error
 "trap INT64_MIN / -1"
 (:/ (int/s64 "-0x8000_0000_0000_0000") -1))

# int/s64 and int/u64 serialization
(assert (deep= (int/to-bytes (u64 0)) @"\x00\x00\x00\x00\x00\x00\x00\x00"))

(assert (deep= (int/to-bytes (i64 1) :le) @"\x01\x00\x00\x00\x00\x00\x00\x00"))
(assert (deep= (int/to-bytes (i64 1) :be) @"\x00\x00\x00\x00\x00\x00\x00\x01"))
(assert (deep= (int/to-bytes (i64 -1)) @"\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"))
(assert (deep= (int/to-bytes (i64 -5) :be) @"\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFB"))

(assert (deep= (int/to-bytes (u64 1) :le) @"\x01\x00\x00\x00\x00\x00\x00\x00"))
(assert (deep= (int/to-bytes (u64 1) :be) @"\x00\x00\x00\x00\x00\x00\x00\x01"))
(assert (deep= (int/to-bytes (u64 300) :be) @"\x00\x00\x00\x00\x00\x00\x01\x2C"))

# int/s64 int/u64 to existing buffer
(let [buf1 @""
      buf2 @"abcd"]
  (assert (deep= (int/to-bytes (i64 1) :le buf1) @"\x01\x00\x00\x00\x00\x00\x00\x00"))
  (assert (deep= buf1 @"\x01\x00\x00\x00\x00\x00\x00\x00"))
  (assert (deep= (int/to-bytes (u64 300) :be buf2) @"abcd\x00\x00\x00\x00\x00\x00\x01\x2C")))

# int/s64 and int/u64 paramater type checking
(assert-error
 "bad value passed to int/to-bytes"
 (int/to-bytes 1))

(assert-error
  "invalid endianness passed to int/to-bytes"
   (int/to-bytes (u64 0) :little))

(assert-error
  "invalid buffer passed to int/to-bytes"
   (int/to-bytes (u64 0) :little :buffer))


# Dynamic bindings
(setdyn :a 10)
(assert (= 40 (with-dyns [:a 25 :b 15] (+ (dyn :a) (dyn :b)))) "dyn usage 1")
(assert (= 10 (dyn :a)) "dyn usage 2")
(assert (= nil (dyn :b)) "dyn usage 3")
(setdyn :a 100)
(assert (= 100 (dyn :a)) "dyn usage 4")

# Keyword arguments
(defn myfn [x y z &keys {:a a :b b :c c}]
  (+ x y z a b c))

(assert (= (+ ;(range 6)) (myfn 0 1 2 :a 3 :b 4 :c 5)) "keyword args 1")
(assert (= (+ ;(range 6)) (myfn 0 1 2 :a 1 :b 6 :c 5 :d 11)) "keyword args 2")

# Comment macro
(comment 1)
(comment 1 2)
(comment 1 2 3)
(comment 1 2 3 4)

# Parser clone
(def p (parser/new))
(assert (= 7 (parser/consume p "(1 2 3 ")) "parser 1")
(def p2 (parser/clone p))
(parser/consume p2 ") 1 ")
(parser/consume p ") 1 ")
(assert (deep= (parser/status p) (parser/status p2)) "parser 2")
(assert (deep= (parser/state p) (parser/state p2)) "parser 3")

# Parser errors
(defn parse-error [input]
  (def p (parser/new))
  (parser/consume p input)
  (parser/error p))

# Invalid utf-8 sequences
(assert (not= nil (parse-error @"\xc3\x28")) "reject invalid utf-8 symbol")
(assert (not= nil (parse-error @":\xc3\x28")) "reject invalid utf-8 keyword")

# Parser line and column numbers
(defn parser-location [input &opt location]
  (def p (parser/new))
  (parser/consume p input)
  (if location
    (parser/where p ;location)
    (parser/where p)))

(assert (= [1 7] (parser-location @"(+ 1 2)")) "parser location 1")
(assert (= [5 7] (parser-location @"(+ 1 2)" [5])) "parser location 2")
(assert (= [10 10] (parser-location @"(+ 1 2)" [10 10])) "parser location 3")

# String check-set
(assert (string/check-set "abc" "a") "string/check-set 1")
(assert (not (string/check-set "abc" "z")) "string/check-set 2")
(assert (string/check-set "abc" "abc") "string/check-set 3")
(assert (string/check-set "abc" "") "string/check-set 4")
(assert (not (string/check-set "" "aabc")) "string/check-set 5")
(assert (not (string/check-set "abc" "abcdefg")) "string/check-set 6")

# Marshal and unmarshal pegs
(def p (-> "abcd" peg/compile marshal unmarshal))
(assert (peg/match p "abcd") "peg marshal 1")
(assert (peg/match p "abcdefg") "peg marshal 2")
(assert (not (peg/match p "zabcdefg")) "peg marshal 3")

# This should be valgrind clean.
(var pegi 3)
(defn marshpeg [p]
  (assert (-> p peg/compile marshal unmarshal) (string "peg marshal " (++ pegi))))
(marshpeg '(* 1 2 (set "abcd") "asdasd" (+ "." 3)))
(marshpeg '(% (* (+ 1 2 3) (* "drop" "bear") '"hi")))
(marshpeg '(> 123 "abcd"))
(marshpeg '{:main (* 1 "hello" :main)})
(marshpeg '(range "AZ"))
(marshpeg '(if-not "abcdf" 123))
(marshpeg '(error ($)))
(marshpeg '(* "abcd" (constant :hi)))
(marshpeg ~(/ "abc" ,identity))
(marshpeg '(if-not "abcdf" 123))
(marshpeg ~(cmt "abcdf" ,identity))
(marshpeg '(group "abc"))

# Module path expansion
(setdyn :current-file "some-dir/some-file")
(defn test-expand [path temp]
  (string (module/expand-path path temp)))

# Right hand operators
(assert (= (int/s64 (sum (range 10))) (sum (map int/s64 (range 10)))) "right hand operators 1")
(assert (= (int/s64 (product (range 1 10))) (product (map int/s64 (range 1 10)))) "right hand operators 2")
(assert (= (int/s64 15) (bor 10 (int/s64 5)) (bor (int/s64 10) 5)) "right hand operators 3")

(assert (= (test-expand "abc" ":cur:/:all:") "some-dir/abc") "module/expand-path 1")
(assert (= (test-expand "./abc" ":cur:/:all:") "some-dir/abc") "module/expand-path 2")
(assert (= (test-expand "abc/def.txt" ":cur:/:name:") "some-dir/def.txt") "module/expand-path 3")
(assert (= (test-expand "abc/def.txt" ":cur:/:dir:/sub/:name:") "some-dir/abc/sub/def.txt") "module/expand-path 4")
(assert (= (test-expand "/abc/../def.txt" ":all:") "/def.txt") "module/expand-path 5")
(assert (= (test-expand "abc/../def.txt" ":all:") "def.txt") "module/expand-path 6")
(assert (= (test-expand "../def.txt" ":all:") "../def.txt") "module/expand-path 7")
(assert (= (test-expand "../././././abcd/../def.txt" ":all:") "../def.txt") "module/expand-path 8")

# Integer type checks
(assert (compare= 0 (- (int/u64 "1000") 1000)) "subtract from int/u64")

(assert (odd? (int/u64 "1001")) "odd? 1")
(assert (not (odd? (int/u64 "1000"))) "odd? 2")
(assert (odd? (int/s64 "1001")) "odd? 3")
(assert (not (odd? (int/s64 "1000"))) "odd? 4")
(assert (odd? (int/s64 "-1001")) "odd? 5")
(assert (not (odd? (int/s64 "-1000"))) "odd? 6")

(assert (even? (int/u64 "1000")) "even? 1")
(assert (not (even? (int/u64 "1001"))) "even? 2")
(assert (even? (int/s64 "1000")) "even? 3")
(assert (not (even? (int/s64 "1001"))) "even? 4")
(assert (even? (int/s64 "-1000")) "even? 5")
(assert (not (even? (int/s64 "-1001"))) "even? 6")

# integer type operations
(defn modcheck [x y]
  (assert (= (string (mod x y)) (string (mod (int/s64 x) y)))
          (string "int/s64 (mod " x " " y ") expected " (mod x y) ", got "
                  (mod (int/s64 x) y)))
  (assert (= (string (% x y)) (string (% (int/s64 x) y)))
          (string "int/s64 (% " x " " y ") expected " (% x y) ", got "
                  (% (int/s64 x) y))))

(modcheck 1 2)
(modcheck 1 3)
(modcheck 4 2)
(modcheck 4 1)
(modcheck 10 3)
(modcheck 10 -3)
(modcheck -10 3)
(modcheck -10 -3)

# Check for issue #1130
(var d (int/s64 7))
(mod 0 d)

(var d (int/s64 7))
(def result (seq [n :in (range -21 0)] (mod n d)))
(assert (deep= result (map int/s64 @[0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6])) "issue #1130")
(start-suite 7)

# Using a large test grammar

(def- specials {'fn true
               'var true
               'do true
               'while true
               'def true
               'splice true
               'set true
               'unquote true
               'quasiquote true
               'quote true
               'if true})

(defn- check-number [text] (and (scan-number text) text))

(defn capture-sym
  [text]
  (def sym (symbol text))
  [(if (or (root-env sym) (specials sym)) :coresym :symbol) text])

(def grammar
  ~{:ws (set " \v\t\r\f\n\0")
    :readermac (set "';~,")
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_|"))
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrvzf0e\"\\")
                       (* "x" :hex :hex)
                       (error (constant "bad hex escape"))))
    :comment (/ '(* "#" (any (if-not (+ "\n" -1) 1))) (constant :comment))
    :symbol (/ ':token ,capture-sym)
    :keyword (/ '(* ":" (any :symchars)) (constant :keyword))
    :constant (/ '(+ "true" "false" "nil") (constant :constant))
    :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
    :string (/ ':bytes (constant :string))
    :buffer (/ '(* "@" :bytes) (constant :string))
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (cmt (* (not (> -1 "`")) (-> :n) '(backmatch :n)) ,=)
                 :main (drop (* :open (any (if-not :close 1)) :close))}
    :long-string (/ ':long-bytes (constant :string))
    :long-buffer (/ '(* "@" :long-bytes) (constant :string))
    :number (/ (cmt ':token ,check-number) (constant :number))
    :raw-value (+ :comment :constant :number :keyword
                  :string :buffer :long-string :long-buffer
                  :parray :barray :ptuple :btuple :struct :dict :symbol)
    :value (* (? '(some (+ :ws :readermac))) :raw-value '(any :ws))
    :root (any :value)
    :root2 (any (* :value :value))
    :ptuple (* '"(" :root (+ '")" (error "")))
    :btuple (* '"[" :root (+ '"]" (error "")))
    :struct (* '"{" :root2 (+ '"}" (error "")))
    :parray (* '"@" :ptuple)
    :barray (* '"@" :btuple)
    :dict (* '"@"  :struct)
    :main (+ :root (error ""))})

(def p (peg/compile grammar))

# Just make sure is valgrind clean.
(def p (-> p make-image load-image))

(assert (peg/match p "abc") "complex peg grammar 1")
(assert (peg/match p "[1 2 3 4]") "complex peg grammar 2")

#
# fn compilation special
#
(defn myfn1 [[x y z] & more]
  more)
(defn myfn2 [head & more]
  more)
(assert (= (myfn1 [1 2 3] 4 5 6) (myfn2 [:a :b :c] 4 5 6)) "destructuring and varargs")

#
# Test propagation of signals via fibers
#

(def f (fiber/new (fn [] (error :abc) 1) :ei))
(def res (resume f))
(assert-error :abc (propagate res f) "propagate 1")

# table/clone

(defn check-table-clone [x msg]
  (assert (= (table/to-struct x) (table/to-struct (table/clone x))) msg))

(check-table-clone @{:a 123 :b 34 :c :hello : 945 0 1 2 3 4 5} "table/clone 1")
(check-table-clone @{} "table/clone 1")

# Make sure Carriage Returns don't end up in doc strings.

(assert (not (string/find "\r" (get ((fiber/getenv (fiber/current)) 'cond) :doc ""))) "no \\r in doc strings")

# module/expand-path regression
(with-dyns [:syspath ".janet/.janet"]
  (assert (= (string (module/expand-path "hello" ":sys:/:all:.janet"))
             ".janet/.janet/hello.janet") "module/expand-path 1"))

# comp should be variadic
(assert (= 10 ((comp +) 1 2 3 4)) "variadic comp 1")
(assert (= 11 ((comp inc +) 1 2 3 4)) "variadic comp 2")
(assert (= 12 ((comp inc inc +) 1 2 3 4)) "variadic comp 3")
(assert (= 13 ((comp inc inc inc +) 1 2 3 4)) "variadic comp 4")
(assert (= 14 ((comp inc inc inc inc +) 1 2 3 4)) "variadic comp 5")
(assert (= 15 ((comp inc inc inc inc inc +) 1 2 3 4)) "variadic comp 6")
(assert (= 16 ((comp inc inc inc inc inc inc +) 1 2 3 4)) "variadic comp 7")

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

# Simple function break
(debug/fbreak map 1)
(def f (fiber/new (fn [] (map inc [1 2 3])) :a))
(resume f)
(assert (= :debug (fiber/status f)) "debug/fbreak")
(debug/unfbreak map 1)
(map inc [1 2 3])

(defn idx= [x y] (= (tuple/slice x) (tuple/slice y)))

# Simple take, drop, etc. tests.
(assert (idx= (take 10 (range 100)) (range 10)) "take 10")
(assert (idx= (drop 10 (range 100)) (range 10 100)) "drop 10")

# Printing to buffers
(def out-buf @"")
(def err-buf @"")
(with-dyns [:out out-buf :err err-buf]
  (print "Hello")
  (prin "hi")
  (eprint "Sup")
  (eprin "not much."))

(assert (= (string out-buf) "Hello\nhi") "print and prin to buffer 1")
(assert (= (string err-buf) "Sup\nnot much.") "eprint and eprin to buffer 1")

# Printing to functions
(def out-buf @"")
(defn prepend [x]
  (with-dyns [:out out-buf]
    (prin "> " x)))
(with-dyns [:out prepend]
  (print "Hello world"))

(assert (= (string out-buf) "> Hello world\n") "print to buffer via function")

(assert (= (string '()) (string [])) "empty bracket tuple literal")

# with-vars
(var abc 123)
(assert (= 356 (with-vars [abc 456] (- abc 100))) "with-vars 1")
(assert-error "with-vars 2" (with-vars [abc 456] (error :oops)))
(assert (= abc 123) "with-vars 3")

# Trim empty string
(assert (= "" (string/trim " ")) "string/trim regression")

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

# OS Date test

(assert (deep= {:year-day 0
                :minutes 30
                :month 0
                :dst false
                :seconds 0
                :year 2014
                :month-day 0
                :hours 20 
                :week-day 3}
               (os/date 1388608200)) "os/date")

# OS mktime test

(assert (= 1388608200 (os/mktime {:year-day 0
                                  :minutes 30
                                  :month 0
                                  :dst false
                                  :seconds 0
                                  :year 2014
                                  :month-day 0
                                  :hours 20
                                  :week-day 3})) "os/mktime")

(def now (os/time))
(assert (= (os/mktime (os/date now)) now) "UTC os/mktime")
(assert (= (os/mktime (os/date now true) true) now) "local os/mktime")
(assert (= (os/mktime {:year 1970}) 0) "os/mktime default values")

# OS strftime test

(assert (= (os/strftime "%Y-%m-%d %H:%M:%S" 0) "1970-01-01 00:00:00") "strftime UTC epoch")
(assert (= (os/strftime "%Y-%m-%d %H:%M:%S" 1388608200) "2014-01-01 20:30:00") "strftime january 2014")
(assert (= (try (os/strftime "%%%d%t") ([err] err)) "invalid conversion specifier '%t'") "invalid conversion specifier")

# Appending buffer to self

(with-dyns [:out @""]
  (prin "abcd")
  (prin (dyn :out))
  (prin (dyn :out))
  (assert (deep= (dyn :out) @"abcdabcdabcdabcd") "print buffer to self"))

(os/setenv "TESTENV1" "v1")
(os/setenv "TESTENV2" "v2")
(assert (= (os/getenv "TESTENV1") "v1") "getenv works")
(def environ (os/environ))
(assert (= [(environ "TESTENV1") (environ "TESTENV2")] ["v1" "v2"]) "environ works")

# Issue #183 - just parse it :)
1e-4000000000000000000000

# Ensure randomness puts n of pred into our buffer eventually
(defn cryptorand-check
  [n pred]
  (def max-attempts 10000)
  (var attempts 0)
  (while (not= attempts max-attempts)
    (def cryptobuf (os/cryptorand 10))
    (when (= n (count pred cryptobuf))
      (break))
    (++ attempts))
  (not= attempts max-attempts))

(def v (math/rng-int (math/rng (os/time)) 100))
(assert (cryptorand-check 0 |(= $ v)) "cryptorand skips value sometimes")
(assert (cryptorand-check 1 |(= $ v)) "cryptorand has value sometimes")

(do 
  (def buf (buffer/new-filled 1))
  (os/cryptorand 1 buf)
  (assert (= (in buf 0) 0) "cryptorand doesn't overwrite buffer")
  (assert (= (length buf) 2) "cryptorand appends to buffer"))

# Nested quasiquotation

(def nested ~(a ~(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(assert (deep= nested '(a ~(b ,(+ 1 2) ,(foo 4 d) e) f)) "nested quasiquote")

# Top level unquote
(defn constantly
  []
  (comptime (math/random)))

(assert (= (constantly) (constantly)) "comptime 1")

(assert-error "arity issue in macro" (eval '(each [])))
(assert-error "comptime issue" (eval '(comptime (error "oops"))))

(with [f (file/temp)]
  (assert (= 0 (file/tell f)) "start of file")
  (file/write f "foo\n")
  (assert (= 4 (file/tell f)) "after written string")
  (file/flush f)
  (file/seek f :set 0)
  (assert (= 0 (file/tell f)) "start of file again")
  (assert (= (string (file/read f :all)) "foo\n") "temp files work"))

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

(assert (pos? (length (gensym))) "gensym not empty, regression #753")


# os/clock. These tests might prove fragile under CI because they
# rely on measured time. We'll see.

(defmacro measure-time [clocks & body]
  (def [t1 t2] [(gensym) (gensym)])
  ~(do
    (def ,t1 (map |(os/clock $) ,clocks))
    ,;body
    (def ,t2 (map |(os/clock $) ,clocks))
    (zipcoll ,clocks (map |(- ;$) (map tuple ,t2 ,t1))))
)

# Spin for 0.1 seconds
(def dt (measure-time [:realtime :monotonic :cputime]
  (def t1 (os/clock :monotonic))
  (while (< (- (os/clock :monotonic) t1) 0.1) true)))
(assert (> (dt :monotonic) 0.10))
(assert (> (dt :cputime) 0.05))

# Sleep for 0.1 seconds
(def dt (measure-time [:realtime :monotonic :cputime] (os/sleep 0.1)))
(assert (> (dt :monotonic) 0.10))
(assert (< (dt :cputime) 0.05))
(start-suite 8)

###
### Compiling brainfuck to Janet.
###

(def- bf-peg
  "Peg for compiling brainfuck into a Janet source ast."
  (peg/compile
    ~{:+ (/ '(some "+") ,(fn [x] ~(+= (DATA POS) ,(length x))))
      :- (/ '(some "-") ,(fn [x] ~(-= (DATA POS) ,(length x))))
      :> (/ '(some ">") ,(fn [x] ~(+= POS ,(length x))))
      :< (/ '(some "<") ,(fn [x] ~(-= POS ,(length x))))
      :. (* "." (constant (prinf "%c" (get DATA POS))))
      :loop (/ (* "[" :main "]") ,(fn [& captures]
                                    ~(while (not= (get DATA POS) 0)
                                       ,;captures)))
      :main (any (+ :s :loop :+ :- :> :< :.))}))

(defn bf
  "Run brainfuck."
  [text]
  (eval
    ~(let [DATA (array/new-filled 100 0)]
       (var POS 50)
       ,;(peg/match bf-peg text))))

(defn test-bf
  "Test some bf for expected output."
  [input output]
  (def b @"")
  (with-dyns [:out b]
    (bf input))
  (assert (= (string output) (string b))
          (string "bf input '"
                  input
                  "' failed, expected "
                  (describe output)
                  ", got "
                  (describe (string b))
                  ".")))

(test-bf "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." "Hello World!\n")

(test-bf ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->
+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."
         "Hello World!\n")

(test-bf "+[+[<<<+>>>>]+<-<-<<<+<++]<<.<++.<++..+++.<<++.<---.>>.>.+++.------.>-.>>--."
         "Hello, World!")

# Prompts and Labels

(assert (= 10 (label a (for i 0 10 (if (= i 5) (return a 10))))) "label 1")

(defn recur
  [lab x y]
  (when (= x y) (return lab :done))
  (def res (label newlab (recur (or lab newlab) (+ x 1) y)))
  (if lab :oops res))
(assert (= :done (recur nil 0 10)) "label 2")

(assert (= 10 (prompt :a (for i 0 10 (if (= i 5) (return :a 10))))) "prompt 1")

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

# Match checks

(assert (= :hi (match nil nil :hi)) "match 1")
(assert (= :hi (match {:a :hi} {:a a} a)) "match 2")
(assert (= nil (match {:a :hi} {:a a :b b} a)) "match 3")
(assert (= nil (match [1 2] [a b c] a)) "match 4")
(assert (= 2 (match [1 2] [a b] b)) "match 5")
(assert (= [2 :a :b] (match [1 2 :a :b] [o & rest] rest)) "match 6")
(assert (= [] (match @[:a] @[x & r] r :fallback)) "match 7")
(assert (= :fallback (match @[1] @[x y & r] r :fallback)) "match 8")
(assert (= [1 2 3 4] (match @[1 2 3 4] @[x y z & r] [x y z ;r] :fallback)) "match 9")

# And/or checks

(assert (= false (and false false)) "and 1")
(assert (= false (or false false)) "or 1")

# #300 Regression test

# Just don't segfault
(assert (peg/match '{:main (replace "S" {"S" :spade})} "S7") "regression #300")

# Test cases for #293
(assert (= :yes (match [1 2 3] [_ a _] :yes :no)) "match wildcard 1")
(assert (= :no (match [1 2 3] [__ a __] :yes :no)) "match wildcard 2")
(assert (= :yes (match [1 2 [1 2 3]] [_ a [_ _ _]] :yes :no)) "match wildcard 3")
(assert (= :yes (match [1 2 3] (_ (even? 2)) :yes :no)) "match wildcard 4")
(assert (= :yes (match {:a 1} {:a _} :yes :no)) "match wildcard 5")
(assert (= false (match {:a 1 :b 2 :c 3} {:a a :b _ :c _ :d _} :no {:a _ :b _ :c _} false :no)) "match wildcard 6")
(assert (= nil (match {:a 1 :b 2 :c 3} {:a a :b _ :c _ :d _} :no {:a _ :b _ :c _} nil :no)) "match wildcard 7")
(assert (= "t" (match [true nil] [true _] "t")) "match wildcard 8")

# Regression #301
(def b (buffer/new-filled 128 0x78))
(assert (= 38 (length (buffer/blit @"" b -1 90))) "buffer/blit 1")

(def a @"abcdefghijklm")
(assert (deep= @"abcde" (buffer/blit @"" a -1 0 5)) "buffer/blit 2")
(assert (deep= @"bcde" (buffer/blit @"" a -1 1 5)) "buffer/blit 3")
(assert (deep= @"cde" (buffer/blit @"" a -1 2 5)) "buffer/blit 4")
(assert (deep= @"de" (buffer/blit @"" a -1 3 5)) "buffer/blit 5")

# chr
(assert (= (chr "a") 97) "chr 1")

# Detaching closure over non resumable fiber.
(do
  (defn f1
    [a]
    (defn f1 [] (++ (a 0)))
    (defn f2 [] (++ (a 0)))
    (error [f1 f2]))
  (def [_ [f1 f2]] (protect (f1 @[0])))
  # At time of writing, mark phase can detach closure envs.
  (gccollect)
  (assert (= 1 (f1)) "detach-non-resumable-closure 1")
  (assert (= 2 (f2)) "detach-non-resumable-closure 2"))

# Marshal closure over non resumable fiber.
(do
  (defn f1
    [a]
    (defn f1 [] (++ (a 0)))
    (defn f2 [] (++ (a 0)))
    (error [f1 f2]))
  (def [_ tup] (protect (f1 @[0])))
  (def [f1 f2] (unmarshal (marshal tup make-image-dict) load-image-dict))
  (assert (= 1 (f1)) "marshal-non-resumable-closure 1")
  (assert (= 2 (f2)) "marshal-non-resumable-closure 2"))

# Marshal closure over currently alive fiber.
(do
  (defn f1
    [a]
    (defn f1 [] (++ (a 0)))
    (defn f2 [] (++ (a 0)))
    (marshal [f1 f2] make-image-dict))
  (def [f1 f2] (unmarshal (f1 @[0]) load-image-dict))
  (assert (= 1 (f1)) "marshal-live-closure 1")
  (assert (= 2 (f2)) "marshal-live-closure 2"))

(do
  (var a 1)
  (defn b [x] (+ a x))
  (def c (unmarshal (marshal b)))
  (assert (= 2 (c 1)) "marshal-on-stack-closure 1"))

# Reduce2

(assert (= (reduce + 0 (range 1 10)) (reduce2 + (range 10))) "reduce2 1")
(assert (= (reduce * 1 (range 2 10)) (reduce2 * (range 1 10))) "reduce2 2")
(assert (= nil (reduce2 * [])) "reduce2 3")

# Accumulate

(assert (deep= (accumulate + 0 (range 5)) @[0 1 3 6 10]) "accumulate 1")
(assert (deep= (accumulate2 + (range 5)) @[0 1 3 6 10]) "accumulate2 1")
(assert (deep= @[] (accumulate2 + [])) "accumulate2 2")
(assert (deep= @[] (accumulate 0 + [])) "accumulate 2")

# Perm strings

(assert (= (os/perm-int "rwxrwxrwx") 8r777) "perm 1")
(assert (= (os/perm-int "rwxr-xr-x") 8r755) "perm 2")
(assert (= (os/perm-int "rw-r--r--") 8r644) "perm 3")

(assert (= (band (os/perm-int "rwxrwxrwx") 8r077) 8r077) "perm 4")
(assert (= (band (os/perm-int "rwxr-xr-x") 8r077) 8r055) "perm 5")
(assert (= (band (os/perm-int "rw-r--r--") 8r077) 8r044) "perm 6")

(assert (= (os/perm-string 8r777) "rwxrwxrwx") "perm 7")
(assert (= (os/perm-string 8r755) "rwxr-xr-x") "perm 8")
(assert (= (os/perm-string 8r644) "rw-r--r--") "perm 9")

# Issue #336 cases - don't segfault

(assert-error "unmarshal errors 1" (unmarshal @"\xd6\xb9\xb9"))
(assert-error "unmarshal errors 2" (unmarshal @"\xd7bc"))
(assert-error "unmarshal errors 3" (unmarshal "\xd3\x01\xd9\x01\x62\xcf\x03\x78\x79\x7a" load-image-dict))
(assert-error "unmarshal errors 4"
              (unmarshal
                @"\xD7\xCD\0e/p\x98\0\0\x03\x01\x01\x01\x02\0\0\x04\0\xCEe/p../tools
\0\0\0/afl\0\0\x01\0erate\xDE\xDE\xDE\xDE\xDE\xDE\xDE\xDE\xDE\xDE
\xA8\xDE\xDE\xDE\xDE\xDE\xDE\0\0\0\xDE\xDE_unmarshal_testcase3.ja
neldb\0\0\0\xD8\x05printG\x01\0\xDE\xDE\xDE'\x03\0marshal_tes/\x02
\0\0\0\0\0*\xFE\x01\04\x02\0\0'\x03\0\r\0\r\0\r\0\r" load-image-dict))

(gccollect)

# in vs get regression
(assert (nil? (first @"")) "in vs get 1")
(assert (nil? (last @"")) "in vs get 1")

# For undefined behavior sanitizer
0xf&1fffFFFF

# Tuple comparison
(assert (< [1 2 3] [2 2 3]) "tuple comparison 1")
(assert (< [1 2 3] [2 2]) "tuple comparison 2")
(assert (< [1 2 3] [2 2 3 4]) "tuple comparison 3")
(assert (< [1 2 3] [1 2 3 4]) "tuple comparison 4")
(assert (< [1 2 3] [1 2 3 -1]) "tuple comparison 5")
(assert (> [1 2 3] [1 2]) "tuple comparison 6")

# Lenprefix rule

(def peg (peg/compile ~(* (lenprefix (/ (* '(any (if-not ":" 1)) ":") ,scan-number) 1) -1)))

(assert (peg/match peg "5:abcde") "lenprefix 1")
(assert (not (peg/match peg "5:abcdef")) "lenprefix 2")
(assert (not (peg/match peg "5:abcd")) "lenprefix 3")

# Packet capture

(def peg2
  (peg/compile
    ~{# capture packet length in tag :header-len
      :packet-header (* (/ ':d+ ,scan-number :header-len) ":")

      # capture n bytes from a backref :header-len
      :packet-body '(lenprefix (-> :header-len) 1)

      # header, followed by body, and drop the :header-len capture
      :packet (/ (* :packet-header :packet-body) ,|$1)

      # any exact seqence of packets (no extra characters)
      :main (* (any :packet) -1)}))

(assert (deep= @["a" "bb" "ccc"] (peg/match peg2 "1:a2:bb3:ccc")) "lenprefix 4")
(assert (deep= @["a" "bb" "cccccc"] (peg/match peg2 "1:a2:bb6:cccccc")) "lenprefix 5")
(assert (= nil (peg/match peg2 "1:a2:bb:5:cccccc")) "lenprefix 6")
(assert (= nil (peg/match peg2 "1:a2:bb:7:cccccc")) "lenprefix 7")

# Regression #400
(assert (= nil (while (and false false) (fn []) (error "should not happen"))) "strangeloop 1")
(assert (= nil (while (not= nil nil) (fn []) (error "should not happen"))) "strangeloop 2")

# Issue #412
(assert (peg/match '(* "a" (> -1 "a") "b") "abc") "lookhead does not move cursor")

(def peg3
  ~{:main (* "(" (thru ")"))})

(def peg4 (peg/compile ~(* (thru "(") '(to ")"))))

(assert (peg/match peg3 "(12345)") "peg thru 1")
(assert (not (peg/match peg3 " (12345)")) "peg thru 2")
(assert (not (peg/match peg3 "(12345")) "peg thru 3")

(assert (= "abc" (0 (peg/match peg4 "123(abc)"))) "peg thru/to 1")
(assert (= "abc" (0 (peg/match peg4 "(abc)"))) "peg thru/to 2")
(assert (not (peg/match peg4 "123(abc")) "peg thru/to 3")

(def peg5 (peg/compile [3 "abc"]))

(assert (:match peg5 "abcabcabc") "repeat alias 1")
(assert (:match peg5 "abcabcabcac") "repeat alias 2")
(assert (not (:match peg5 "abcabc")) "repeat alias 3")

(defn check-jdn [x]
  (assert (deep= (parse (string/format "%j" x)) x) "round trip jdn"))

(check-jdn 0)
(check-jdn nil)
(check-jdn [])
(check-jdn @[[] [] 1231 9.123123 -123123 0.1231231230001])
(check-jdn -0.123123123123)
(check-jdn 12837192371923)
(check-jdn "a string")
(check-jdn @"a buffer")

# Issue 428
(var result nil)
(defn f [] (yield {:a :ok}))
(assert-no-error "issue 428 1" (loop [{:a x} :in (fiber/new f)] (set result x)))
(assert (= result :ok) "issue 428 2")

# Inline 3 argument get
(assert (= 10 (do (var a 10) (set a (get '{} :a a)))) "inline get 1")

# Keyword and Symbol slice
(assert (= :keyword (keyword/slice "some_keyword_slice" 5 12)) "keyword slice")
(assert (= 'symbol (symbol/slice "some_symbol_slice" 5 11)) "symbol slice")

# Peg find and find-all
(def p "/usr/local/bin/janet")
(assert (= (peg/find '"n/" p) 13) "peg find 1")
(assert (not (peg/find '"t/" p)) "peg find 2")
(assert (deep= (peg/find-all '"/" p) @[0 4 10 14]) "peg find-all")

# Peg replace and replace-all
(defn check-replacer
  [x y z]
  (assert (= (string/replace x y z) (string (peg/replace x y z))) "replacer test replace")
  (assert (= (string/replace-all x y z) (string (peg/replace-all x y z))) "replacer test replace-all"))
(check-replacer "abc" "Z" "abcabcabcabasciabsabc")
(check-replacer "abc" "Z" "")
(check-replacer "aba" "ZZZZZZ" "ababababababa")
(check-replacer "aba" "" "ababababababa")
(check-replacer "aba" string/ascii-upper "ababababababa")
(check-replacer "aba" 123 "ababababababa")

(assert (= (string (peg/replace-all ~(set "ab") string/ascii-upper "abcaa"))
           "ABcAA")
        "peg/replace-all cfunction")
(assert (= (string (peg/replace-all ~(set "ab") |$ "abcaa"))
           "abcaa")
        "peg/replace-all function")

(defn peg-test [name f peg subst text expected]
  (assert (= (string (f peg subst text)) expected) name))

(peg-test "peg/replace has access to captures"
  peg/replace
  ~(sequence "." (capture (set "ab")))
  (fn [str char] (string/format "%s -> %s, " str (string/ascii-upper char)))
  ".a.b.c"
  ".a -> A, .b.c")

(peg-test "peg/replace-all has access to captures"
  peg/replace-all
  ~(sequence "." (capture (set "ab")))
  (fn [str char] (string/format "%s -> %s, " str (string/ascii-upper char)))
  ".a.b.c"
  ".a -> A, .b -> B, .c")

# Peg bug
(assert (deep= @[] (peg/match '(any 1) @"")) "peg empty pattern 1")
(assert (deep= @[] (peg/match '(any 1) (buffer))) "peg empty pattern 2")
(assert (deep= @[] (peg/match '(any 1) "")) "peg empty pattern 3")
(assert (deep= @[] (peg/match '(any 1) (string))) "peg empty pattern 4")
(assert (deep= @[] (peg/match '(* "test" (any 1)) @"test")) "peg empty pattern 5")
(assert (deep= @[] (peg/match '(* "test" (any 1)) (buffer "test"))) "peg empty pattern 6")

# number pattern
(assert (deep= @[111] (peg/match '(number :d+) "111")) "simple number capture 1")
(assert (deep= @[255] (peg/match '(number :w+) "0xff")) "simple number capture 2")

# quoted match test
(assert (= :yes (match 'john 'john :yes _ :nope)) "quoted literal match 1")
(assert (= :nope (match 'john ''john :yes _ :nope)) "quoted literal match 2")
(start-suite 9)

# Subprocess

(def janet (dyn :executable))

(repeat 10

  (let [p (os/spawn [janet "-e" `(print "hello")`] :p {:out :pipe})]
    (os/proc-wait p)
    (def x (:read (p :out) :all))
    (assert (deep= "hello" (string/trim x)) "capture stdout from os/spawn pre close."))

  (let [p (os/spawn [janet "-e" `(print "hello")`] :p {:out :pipe})]
    (def x (:read (p :out) 1024))
    (os/proc-wait p)
    (assert (deep= "hello" (string/trim x)) "capture stdout from os/spawn post close."))

  (let [p (os/spawn [janet "-e" `(file/read stdin :line)`] :px {:in :pipe})]
    (:write (p :in) "hello!\n")
    (assert-no-error "pipe stdin to process" (os/proc-wait p))))

(let [p (os/spawn [janet "-e" `(print (file/read stdin :line))`] :px {:in :pipe :out :pipe})]
  (:write (p :in) "hello!\n")
  (def x (:read (p :out) 1024))
  (assert-no-error "pipe stdin to process 2" (os/proc-wait p))
  (assert (= "hello!" (string/trim x)) "round trip pipeline in process"))

(let [p (os/spawn [janet "-e" `(do (ev/sleep 30) (os/exit 24)`] :p)]
  (os/proc-kill p)
  (def retval (os/proc-wait p))
  (assert (not= retval 24) "Process was *not* terminated by parent"))

(let [p (os/spawn [janet "-e" `(do (ev/sleep 30) (os/exit 24)`] :p)]
  (os/proc-kill p false :term)
  (def retval (os/proc-wait p))
  (assert (not= retval 24) "Process was *not* terminated by parent"))

# Parallel subprocesses

(defn calc-1
  "Run subprocess, read from stdout, then wait on subprocess."
  [code]
  (let [p (os/spawn [janet "-e" (string `(printf "%j" ` code `)`)] :px {:out :pipe})]
    (os/proc-wait p)
    (def output (:read (p :out) :all))
    (parse output)))

(assert
  (deep=
    (ev/gather
      (calc-1 "(+ 1 2 3 4)")
      (calc-1 "(+ 5 6 7 8)")
      (calc-1 "(+ 9 10 11 12)"))
    @[10 26 42]) "parallel subprocesses 1")

(defn calc-2
  "Run subprocess, wait on subprocess, then read from stdout. Read only up to 10 bytes instead of :all"
  [code]
  (let [p (os/spawn [janet "-e" (string `(printf "%j" ` code `)`)] :px {:out :pipe})]
    (def output (:read (p :out) 10))
    (os/proc-wait p)
    (parse output)))

(assert
  (deep=
    (ev/gather
      (calc-2 "(+ 1 2 3 4)")
      (calc-2 "(+ 5 6 7 8)")
      (calc-2 "(+ 9 10 11 12)"))
    @[10 26 42]) "parallel subprocesses 2")

# File piping

(assert-no-error "file writing 1"
  (with [f (file/temp)]
    (os/execute [janet "-e" `(repeat 20 (print :hello))`] :p {:out f})))

(assert-no-error "file writing 2"
  (with [f (file/open "unique.txt" :w)]
    (os/execute [janet "-e" `(repeat 20 (print :hello))`] :p {:out f})
    (file/flush f)))

# each-line iterator

(assert-no-error "file/lines iterator"
   (def outstream (os/open "unique.txt" :wct))
   (def buf1 "123\n456\n")
   (defer (:close outstream)
     (:write outstream buf1))
   (var buf2 "")
   (with [f (file/open "unique.txt" :r)]
     (each line (file/lines f)
        (set buf2 (string buf2 line))))
   (assert (= buf1 buf2) "file/lines iterator")
   (os/rm "unique.txt"))

# Issue #593
(assert-no-error "file writing 3"
  (def outfile (file/open "unique.txt" :w))
  (os/execute [janet "-e" "(pp (seq [i :range (1 10)] i))"] :p {:out outfile})
  (file/flush outfile)
  (file/close outfile)
  (os/rm "unique.txt"))

# Ensure that the stream created by os/open works

(assert-no-error "File writing 4.1"
   (def outstream (os/open "unique.txt" :wct))
   (defer (:close outstream)
     (:write outstream "123\n")
     (:write outstream "456\n"))
   # Cast to string to enable comparison
   (assert (= "123\n456\n" (string (slurp "unique.txt"))) "File writing 4.2")
   (os/rm "unique.txt"))

# Test that the stream created by os/open can be read from
(comment
  (assert-no-error "File reading 1.1"
    (def outstream (os/open "unique.txt" :wct))
    (defer (:close outstream)
      (:write outstream "123\n")
      (:write outstream "456\n"))

    (def outstream (os/open "unique.txt" :r))
    (defer (:close outstream)
      (assert (= "123\n456\n" (string (:read outstream :all))) "File reading 1.2"))
    (os/rm "unique.txt")))

  # ev/gather

(assert (deep= @[1 2 3] (ev/gather 1 2 3)) "ev/gather 1")
(assert (deep= @[] (ev/gather)) "ev/gather 2")
(assert-error "ev/gather 3" (ev/gather 1 2 (error 3)))

# Net testing

(repeat 10

  (defn handler
    "Simple handler for connections."
    [stream]
    (defer (:close stream)
      (def id (gensym))
      (def b @"")
      (net/read stream 1024 b)
      (net/write stream b)
      (buffer/clear b)))

  (def s (net/server "127.0.0.1" "8000" handler))
  (assert s "made server 1")

  (defn test-echo [msg]
    (with [conn (net/connect "127.0.0.1" "8000")]
      (net/write conn msg)
      (def res (net/read conn 1024))
      (assert (= (string res) msg) (string "echo " msg))))

  (test-echo "hello")
  (test-echo "world")
  (test-echo (string/repeat "abcd" 200))

  (:close s))

# Test on both server and client
(defn names-handler
  [stream]
  (defer (:close stream)
    # prevent immediate close
    (ev/read stream 1)
    (def [host port] (net/localname stream))
    (assert (= host "127.0.0.1") "localname host server")
    (assert (= port 8000) "localname port server")))

# Test localname and peername
(repeat 10
  (with [s (net/server "127.0.0.1" "8000" names-handler)]
    (repeat 10
      (with [conn (net/connect "127.0.0.1" "8000")]
        (def [host port] (net/peername conn))
        (assert (= host "127.0.0.1") "peername host client ")
        (assert (= port 8000) "peername port client")
        # let server close
        (ev/write conn " "))))
  (gccollect))

# Create pipe

(var pipe-counter 0)
(def chan (ev/chan 10))
(let [[reader writer] (os/pipe)]
  (ev/spawn
    (while (ev/read reader 3)
      (++ pipe-counter))
    (assert (= 20 pipe-counter) "ev/pipe 1")
    (ev/give chan 1))

  (for i 0 10
    (ev/write writer "xxx---"))

  (ev/close writer)
  (ev/take chan))

(var result nil)
(var fiber nil)
(set fiber
  (ev/spawn
    (set result (protect (ev/sleep 10)))
    (assert (= result '(false "boop")) "ev/cancel 1")))
(ev/sleep 0)
(ev/cancel fiber "boop")

(assert (os/execute [janet "-e" `(+ 1 2 3)`] :xp) "os/execute self")

# Test some channel

(def c1 (ev/chan))
(def c2 (ev/chan))
(def arr @[])
(ev/spawn
  (while (def x (ev/take c1))
    (array/push arr x))
  (ev/chan-close c2))
(for i 0 1000
  (ev/give c1 i))
(ev/chan-close c1)
(ev/take c2)
(assert (= (slice arr) (slice (range 1000))) "ev/chan-close 1")

(def c1 (ev/chan))
(def c2 (ev/chan))
(def arr @[])
(ev/spawn
  (while (def x (ev/take c1))
    (array/push arr x))
  (ev/sleep 0.1)
  (ev/chan-close c2))
(for i 0 100
  (ev/give c1 i))
(ev/chan-close c1)
(ev/select c2)
(assert (= (slice arr) (slice (range 100))) "ev/chan-close 2")

(def c1 (ev/chan))
(def c2 (ev/chan))
(def arr @[])
(ev/spawn
  (while (def x (ev/take c1))
    (array/push arr x))
  (ev/chan-close c2))
(for i 0 100
  (ev/give c1 i))
(ev/chan-close c1)
(ev/rselect c2)
(assert (= (slice arr) (slice (range 100))) "ev/chan-close 3")

# threaded channels

(def ch (ev/thread-chan 2))
(def att (ev/thread-chan 109))
(assert att "`att` was nil after creation")
(ev/give ch att)
(ev/do-thread
  (assert (ev/take ch) "channel packing bug for threaded abstracts on threaded channels."))

# marshal channels

(def ch (ev/chan 10))
(ev/give ch "hello")
(ev/give ch "world")
(def ch2 (-> ch marshal unmarshal))
(def item1 (ev/take ch2))
(def item2 (ev/take ch2))
(assert (= item1 "hello"))
(assert (= item2 "world"))
(start-suite 10)

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
(assert (= :brackets (tuple/type (1 (macex1 '~[1 2 3 4])))) "macex1 qq bracket tuple")
(assert (deep= (macex1 '~@[1 2 3 4 ,blah]) '~@[1 2 3 4 ,blah]) "macex1 qq array")

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

  (assert (deep= (keep-syntax brak @[1 2 3]) @[1 2 3]) "keep-syntax brackets ignore array")
  (assert (= (keep-syntax! brak @[1 2 3]) '[1 2 3]) "keep-syntax! brackets replace array")

  (assert (= (keep-syntax! par (map inc @[1 2 3])) '(2 3 4)) "keep-syntax! parens coerce array")
  (assert (not= (keep-syntax! brak @[1 2 3]) '(1 2 3)) "keep-syntax! brackets not parens")
  (assert (not= (keep-syntax! par @[1 2 3]) '[1 2 3]) "keep-syntax! parens not brackets")
  (assert (= (tuple/sourcemap brak)
             (tuple/sourcemap (keep-syntax! brak @[1 2 3]))) "keep-syntax! brackets source map")

  (keep-syntax par brak)
  (assert (not= (tuple/sourcemap brak) (tuple/sourcemap par)) "keep-syntax no mutate")
  (assert (= (keep-syntax 1 brak) brak) "keep-syntax brackets ignore type"))

# Cancel test
(def f (fiber/new (fn [&] (yield 1) (yield 2) (yield 3) 4) :yti))
(assert (= 1 (resume f)) "cancel resume 1")
(assert (= 2 (resume f)) "cancel resume 2")
(assert (= :hi (cancel f :hi)) "cancel resume 3")
(assert (= :error (fiber/status f)) "cancel resume 4")

# Curenv
(assert (= (curenv) (curenv 0)) "curenv 1")
(assert (= (table/getproto (curenv)) (curenv 1)) "curenv 2")
(assert (= nil (curenv 1000000)) "curenv 3")
(assert (= root-env (curenv 1)) "curenv 4")

# Import macro test
(assert-no-error "import macro 1" (macex '(import a :as b :fresh maybe)))
(assert (deep= ~(,import* "a" :as "b" :fresh maybe) (macex '(import a :as b :fresh maybe))) "import macro 2")

# #477 walk preserving bracket type
(assert (= :brackets (tuple/type (postwalk identity '[]))) "walk square brackets 1")
(assert (= :brackets (tuple/type (walk identity '[]))) "walk square brackets 2")

# # off by 1 error in inttypes
(assert (= (int/s64 "-0x8000_0000_0000_0000") (+ (int/s64 "0x7FFF_FFFF_FFFF_FFFF") 1)) "int types wrap around")

#
# Longstring indentation
#

(defn reindent
  "Reindent a the contents of a longstring as the Janet parser would.
  This include removing leading and trailing newlines."
  [text indent]

  # Detect minimum indent
  (var rewrite true)
  (each index (string/find-all "\n" text)
    (for i (+ index 1) (+ index indent 1)
      (case (get text i)
        nil (break)
        (chr "\n") (break)
        (chr " ") nil
        (set rewrite false))))

  # Only re-indent if no dedented characters.
  (def str
    (if rewrite
      (peg/replace-all ~(* "\n" (between 0 ,indent " ")) "\n" text)
      text))

  (def first-nl (= (chr "\n") (first str)))
  (def last-nl (= (chr "\n") (last str)))
  (string/slice str (if first-nl 1 0) (if last-nl -2)))

(defn reindent-reference
  "Same as reindent but use parser functionality. Useful for validating conformance."
  [text indent]
  (if (empty? text) (break text))
  (def source-code
    (string (string/repeat " " indent) "``````"
            text
            "``````"))
  (parse source-code))

(var indent-counter 0)
(defn check-indent
  [text indent]
  (++ indent-counter)
  (let [a (reindent text indent)
        b (reindent-reference text indent)]
    (assert (= a b) (string "indent " indent-counter " (indent=" indent ")"))))

(check-indent "" 0)
(check-indent "\n" 0)
(check-indent "\n" 1)
(check-indent "\n\n" 0)
(check-indent "\n\n" 1)
(check-indent "\nHello, world!" 0)
(check-indent "\nHello, world!" 1)
(check-indent "Hello, world!" 0)
(check-indent "Hello, world!" 1)
(check-indent "\n    Hello, world!" 4)
(check-indent "\n    Hello, world!\n" 4)
(check-indent "\n    Hello, world!\n   " 4)
(check-indent "\n    Hello, world!\n    " 4)
(check-indent "\n    Hello, world!\n   dedented text\n    " 4)
(check-indent "\n    Hello, world!\n    indented text\n    " 4)

# String bugs
(assert (deep= (string/find-all "qq" "qqq") @[0 1]) "string/find-all 1")
(assert (deep= (string/find-all "q" "qqq") @[0 1 2]) "string/find-all 2")
(assert (deep= (string/split "qq" "1qqqqz") @["1" "" "z"]) "string/split 1")
(assert (deep= (string/split "aa" "aaa") @["" "a"]) "string/split 2")

# Comparisons
(assert (> 1e23 100) "less than immediate 1")
(assert (> 1e23 1000) "less than immediate 2")
(assert (< 100 1e23) "greater than immediate 1")
(assert (< 1000 1e23) "greater than immediate 2")

# os/execute with environment variables
(assert (= 0 (os/execute [(dyn :executable) "-e" "(+ 1 2 3)"] :pe (merge (os/environ) {"HELLO" "WORLD"}))) "os/execute with env")

# Regression #638
(compwhen
  (dyn 'ev/go)
  (assert
    (= [true :caught]
       (protect
         (try
           (do
             (ev/sleep 0)
             (with-dyns []
               (ev/sleep 0)
               (error "oops")))
           ([err] :caught))))
    "regression #638"))


# Struct prototypes
(def x (struct/with-proto {1 2 3 4} 5 6))
(def y (-> x marshal unmarshal))
(def z {1 2 3 4})
(assert (= 2 (get x 1)) "struct get proto value 1")
(assert (= 4 (get x 3)) "struct get proto value 2")
(assert (= 6 (get x 5)) "struct get proto value 3")
(assert (= x y) "struct proto marshal equality 1")
(assert (= (getproto x) (getproto y)) "struct proto marshal equality 2")
(assert (= 0 (cmp x y)) "struct proto comparison 1")
(assert (= 0 (cmp (getproto x) (getproto y))) "struct proto comparison 2")
(assert (not= (cmp x z) 0) "struct proto comparison 3")
(assert (not= (cmp y z) 0) "struct proto comparison 4")
(assert (not= x z) "struct proto comparison 5")
(assert (not= y z) "struct proto comparison 6")
(assert (= (x 5) 6) "struct proto get 1")
(assert (= (y 5) 6) "struct proto get 1")
(assert (deep= x y) "struct proto deep= 1")
(assert (deep-not= x z) "struct proto deep= 2")
(assert (deep-not= y z) "struct proto deep= 3")

# Issue #751
(def t {:side false})
(assert (nil? (get-in t [:side :note])) "get-in with false value")
(assert (= (get-in t [:side :note] "dflt") "dflt")
        "get-in with false value and default")

(assert (= (math/gcd 462 1071) 21) "math/gcd 1")
(assert (= (math/lcm 462 1071) 23562) "math/lcm 1")

# Evaluate stream with `dofile`
(def [r w] (os/pipe))
(:write w "(setdyn :x 10)")
(:close w)
(def stream-env (dofile r))
(assert (= (stream-env :x) 10) "dofile stream 1")

# Issue #861 - should be valgrind clean
(def step1 "(a b c d)\n")
(def step2 "(a b)\n")
(def p1 (parser/new))
(parser/state p1)
(parser/consume p1 step1)
(loop [v :iterate (parser/produce p1)])
(parser/state p1)
(def p2 (parser/clone p1))
(parser/state p2)
(parser/consume p2 step2)
(loop [v :iterate (parser/produce p2)])
(parser/state p2)

# Check missing struct proto bug.
(assert (struct/getproto (struct/with-proto {:a 1} :b 2 :c nil)) "missing struct proto")

# Test thaw and freeze
(def table-to-freeze @{:c 22 :b [1 2 3 4] :d @"test" :e "test2"})
(def table-to-freeze-with-inline-proto @{:a @[1 2 3] :b @[1 2 3 4] :c 22 :d @"test" :e @"test2"})
(def struct-to-thaw (struct/with-proto {:a [1 2 3]} :c 22 :b [1 2 3 4] :d "test" :e "test2"))
(table/setproto table-to-freeze @{:a @[1 2 3]})
(assert (deep= {:a [1 2 3] :b [1 2 3 4] :c 22 :d "test" :e "test2"} (freeze table-to-freeze)))
(assert (deep= table-to-freeze-with-inline-proto (thaw table-to-freeze)))
(assert (deep= table-to-freeze-with-inline-proto (thaw struct-to-thaw)))
(start-suite 11)

# math gamma

(assert (< 11899423.08 (math/gamma 11.5) 11899423.085) "math/gamma")
(assert (< 2605.1158 (math/log-gamma 500) 2605.1159) "math/log-gamma")

# missing symbols

(defn lookup-symbol [sym] (defglobal sym 10) (dyn sym))

(setdyn :missing-symbol lookup-symbol)

(assert (= (eval-string "(+ a 5)") 15) "lookup missing symbol")

(setdyn :missing-symbol nil)
(setdyn 'a nil)

(assert-error "compile error" (eval-string "(+ a 5)"))

# 919
(defn test
  []
  (var x 1)
  (set x ~(,x ()))
  x)

(assert (= (test) '(1 ())) "issue #919")

(assert (= (hash 0) (hash (* -1 0))) "hash -0 same as hash 0")

# os/execute regressions
(for i 0 10
  (assert (= i (os/execute [(dyn :executable) "-e" (string/format "(os/exit %d)" i)] :p)) (string "os/execute " i)))

# to/thru bug
(def pattern
  (peg/compile
    '{:dd (sequence :d :d)
      :sep (set "/-")
      :date (sequence :dd :sep :dd)
      :wsep (some (set " \t"))
      :entry (group (sequence (capture :date) :wsep (capture :date)))
      :main (some (thru :entry))}))

(def alt-pattern
  (peg/compile
    '{:dd (sequence :d :d)
      :sep (set "/-")
      :date (sequence :dd :sep :dd)
      :wsep (some (set " \t"))
      :entry (group (sequence (capture :date) :wsep (capture :date)))
      :main (some (choice :entry 1))}))

(def text "1800-10-818-9-818 16/12\n17/12 19/12\n20/12 11/01")
(assert (deep= (peg/match pattern text) (peg/match alt-pattern text)) "to/thru bug #971")

(assert-error
  "table rawget regression"
  (table/new -1))

# Named arguments
(defn named-arguments
  [&named bob sally joe]
  (+ bob sally joe))

(assert (= 15 (named-arguments :bob 3 :sally 5 :joe 7)) "named arguments 1")

(defn named-opt-arguments
  [&opt x &named a b c]
  (+ x a b c))

(assert (= 10 (named-opt-arguments 1 :a 2 :b 3 :c 4)) "named arguments 2")

(let [b @""]
  (defn dummy [a b c]
    (+ a b c))
  (trace dummy)
  (defn errout [arg]
    (buffer/push b arg))
  (assert (= 6 (with-dyns [*err* errout] (dummy 1 2 3))) "trace to custom err function")
  (assert (deep= @"trace (dummy 1 2 3)\n" b) "trace buffer correct"))

(def f (asm (disasm (fn [x] (fn [y] (+ x y))))))
(assert (= ((f 10) 37) 47) "asm environment tables")

(end-suite)
(start-suite 12)

(var counter 0)
(def thunk (delay (++ counter)))
(assert (= (thunk) 1) "delay 1")
(assert (= counter 1) "delay 2")
(assert (= (thunk) 1) "delay 3")
(assert (= counter 1) "delay 4")

(def has-ffi (dyn 'ffi/native))

# FFI check
(compwhen has-ffi
  (ffi/context))

(compwhen has-ffi
  (ffi/defbind memcpy :ptr [dest :ptr src :ptr n :size]))
(compwhen has-ffi
  (def buffer1 @"aaaa")
  (def buffer2 @"bbbb")
  (memcpy buffer1 buffer2 4)
  (assert (= (string buffer1) "bbbb") "ffi 1 - memcpy"))

(compwhen has-ffi
  (assert (= 8 (ffi/size [:int :char])) "size unpacked struct 1")
  (assert (= 5 (ffi/size [:pack :int :char])) "size packed struct 1")
  (assert (= 5 (ffi/size [:int :pack-all :char])) "size packed struct 2")
  (assert (= 4 (ffi/align [:int :char])) "align 1")
  (assert (= 1 (ffi/align [:pack :int :char])) "align 2")
  (assert (= 1 (ffi/align [:int :char :pack-all])) "align 3")
  (assert (= 26 (ffi/size [:char :pack :int @[:char 21]])) "array struct size"))

(end-suite)

(start-suite 13)

(assert (deep= (tabseq [i :in (range 3)] i (* 3 i))
               @{0 0 1 3 2 6}))

(assert (deep= (tabseq [i :in (range 3)] i)
               @{}))

(def- sym-prefix-peg
  (peg/compile
    ~{:symchar (+ (range "\x80\xff" "AZ" "az" "09") (set "!$%&*+-./:<?=>@^_"))
      :anchor (drop (cmt ($) ,|(= $ 0)))
      :cap (* (+ (> -1 (not :symchar)) :anchor) (* ($) '(some :symchar)))
      :recur (+ :cap (> -1 :recur))
      :main (> -1 :recur)}))

(assert (deep= (peg/match sym-prefix-peg @"123" 3) @[0 "123"]) "peg lookback")
(assert (deep= (peg/match sym-prefix-peg @"1234" 4) @[0 "1234"]) "peg lookback 2")

(assert (deep= (peg/replace-all '(* (<- 1) 1 (backmatch)) "xxx" "aba cdc efa") @"xxx xxx efa") "peg replace-all 1")

(end-suite)
