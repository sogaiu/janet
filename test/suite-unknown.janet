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

# Set global variables to prevent some possible compiler optimizations
# that defeat point of the test - 2771171
(var zero 0)
(var one 1)
(var two 2)
(var three 3)
(var plus +)
(assert (= 22 (plus one (plus 1 2 two) (plus 8 (plus zero 1) 4 three)))
        "nested function calls")

# String literals - 45f8db0
(assert (= "abcd" "\x61\x62\x63\x64") "hex escapes")
(assert (= "\e" "\x1B") "escape character")
(assert (= "\x09" "\t") "tab character")

# McCarthy's 91 function - 2771171
(var f91 nil)
(set f91 (fn [n]
           (if (> n 100)
             (- n 10)
             (f91 (f91 (+ n 11))))))
(assert (= 91 (f91 10)) "f91(10) = 91")
(assert (= 91 (f91 11)) "f91(11) = 91")
(assert (= 91 (f91 20)) "f91(20) = 91")
(assert (= 91 (f91 31)) "f91(31) = 91")
(assert (= 91 (f91 100)) "f91(100) = 91")
(assert (= 91 (f91 101)) "f91(101) = 91")
(assert (= 92 (f91 102)) "f91(102) = 92")
(assert (= 93 (f91 103)) "f91(103) = 93")
(assert (= 94 (f91 104)) "f91(104) = 94")

# Fibonacci - 23196ff
(def fib
  (do
    (var fib nil)
    (set fib (fn [n]
               (if (< n 2)
                 n
                 (+ (fib (- n 1)) (fib (- n 2))))))))
(def fib2
  (fn fib2 [n]
    (if (< n 2)
      n
      (+ (fib2 (- n 1)) (fib2 (- n 2))))))

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

# Closure in non function scope - 911b0b1
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

# e2c78b3
((fn []
   (var accum 1)
   (var counter 0)
   (while (< counter 16)
     (set accum (blshift accum 1))
     (set counter (+ 1 counter)))
   (assert (= accum 65536) "loop in closure")))

(var accum 1)
(var counter 0)
(while (< counter 16)
  (set accum (blshift accum 1))
  (set counter (+ 1 counter)))
(assert (= accum 65536) "loop globally")

# Symbol function - 5460ff1

(assert (= (symbol "abc" 1 2 3) 'abc123) "symbol function")

# Fiber tests - 21bd960

(def afiber (fiber/new (fn []
                         (def x (yield))
                         (error (string "hello, " x))) :ye))

(resume afiber) # first resume to prime
(def afiber-result (resume afiber "world!"))

(assert (= afiber-result "hello, world!") "fiber error result")
(assert (= (fiber/status afiber) :error) "fiber error status")

# yield tests - 171c0ce

(def t (fiber/new (fn [&] (yield 1) (yield 2) 3)))

(assert (= 1 (resume t)) "initial transfer to new fiber")
(assert (= 2 (resume t)) "second transfer to fiber")
(assert (= 3 (resume t)) "return from fiber")
(assert (= (fiber/status t) :dead) "finished fiber is dead")

#
# Test propagation of signals via fibers - b8032ec61
#

(def f (fiber/new (fn [] (error :abc) 1) :ei))
(def res (resume f))
(assert-error :abc (propagate res f) "propagate 1")

# Cancel test - 28439d822
(def f (fiber/new (fn [&] (yield 1) (yield 2) (yield 3) 4) :yti))
(assert (= 1 (resume f)) "cancel resume 1")
(assert (= 2 (resume f)) "cancel resume 2")
(assert (= :hi (cancel f :hi)) "cancel resume 3")
(assert (= :error (fiber/status f)) "cancel resume 4")

# Var arg tests - f054586

(def vargf (fn [more] (apply + more)))

(assert (= 0 (vargf @[])) "var arg no arguments")
(assert (= 1 (vargf @[1])) "var arg no packed arguments")
(assert (= 3 (vargf @[1 2])) "var arg tuple size 1")
(assert (= 10 (vargf @[1 2 3 4])) "var arg tuple size 2, 2 normal args")
(assert (= 110 (vargf @[1 2 3 4 10 10 10 10 10 10 10 10 10 10]))
        "var arg large tuple")

# Higher order functions - d9f24ef

(def compose (fn [f g] (fn [& xs] (f (apply g xs)))))

(def -+ (compose - +))
(def +- (compose + -))

(assert (= (-+ 1 2 3 4) -10) "compose - +")
(assert (= (+- 1 2 3 4) -8) "compose + -")
(assert (= ((compose -+ +-) 1 2 3 4) 8) "compose -+ +-")
(assert (= ((compose +- -+) 1 2 3 4) 10) "compose +- -+")

# UTF-8 - d9f24ef

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

# Symbols with @ character - d68eae9

(def @ 1)
(assert (= @ 1) "@ symbol")
(def @-- 2)
(assert (= @-- 2) "@-- symbol")
(def @hey 3)
(assert (= @hey 3) "@hey symbol")

# Merge sort - f5b29b8

# Imperative (and verbose) merge sort merge
(defn merge-sort
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

(assert (apply <= (merge-sort @[1 3 5] @[2 4 6])) "merge sort merge 1")
(assert (apply <= (merge-sort @[1 2 3] @[4 5 6])) "merge sort merge 2")
(assert (apply <= (merge-sort @[1 3 5] @[2 4 6 6 6 9])) "merge sort merge 3")
(assert (apply <= (merge-sort '(1 3 5) @[2 4 6 6 6 9])) "merge sort merge 4")

(assert (deep= @[1 2 3 4 5] (sort @[5 3 4 1 2])) "sort 1")
(assert (deep= @[{:a 1} {:a 4} {:a 7}]
               (sort-by |($ :a) @[{:a 4} {:a 7} {:a 1}])) "sort 2")
(assert (deep= @[1 2 3 4 5] (sorted [5 3 4 1 2])) "sort 3")
(assert (deep= @[{:a 1} {:a 4} {:a 7}]
               (sorted-by |($ :a) [{:a 4} {:a 7} {:a 1}])) "sort 4")

# Dynamic defs - ec65f03

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

# 027b2a8

(defn assert-many [f n e]
 (var good true)
 (loop [i :range [0 n]]
  (if (not (f))
   (set good false)))
 (assert good e))

(assert-many (fn [] (>= 1 (math/random) 0)) 200 "(random) between 0 and 1")

# Test max triangle program - c0e373f

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

# Large functions - 6822400
(def manydefs (seq [i :range [0 300]]
                (tuple 'def (gensym) (string "value_" i))))
(array/push manydefs (tuple * 10000 3 5 7 9))
(def f (compile ['do ;manydefs] (fiber/getenv (fiber/current))))
(assert (= (f) (* 10000 3 5 7 9)) "long function compilation")

# Closure in while loop - abe7d59
(def closures (seq [i :range [0 5]] (fn [] i)))
(assert (= 0 ((get closures 0))) "closure in loop 0")
(assert (= 1 ((get closures 1))) "closure in loop 1")
(assert (= 2 ((get closures 2))) "closure in loop 2")
(assert (= 3 ((get closures 3))) "closure in loop 3")
(assert (= 4 ((get closures 4))) "closure in loop 4")

# More numerical tests - e05022f
(assert (= 1 1.0) "numerical equal 1")
(assert (= 0 0.0) "numerical equal 2")
(assert (= 0 -0.0) "numerical equal 3")
(assert (= 2_147_483_647 2_147_483_647.0) "numerical equal 4")
(assert (= -2_147_483_648 -2_147_483_648.0) "numerical equal 5")

# Looping idea - 45f8db0
(def xs
  (seq [x :in [-1 0 1] y :in [-1 0 1] :when (not= x y 0)] (tuple x y)))
(def txs (apply tuple xs))

(assert (= txs [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])
        "nested seq")

# Another regression test - no segfaults - 6b4824c
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

# Regression - issue #24

(def t (put @{} :hi 1))
(assert (deep= t @{:hi 1}) "regression #24")

# Tuple types - c6edf03ae

(assert (= (tuple/type '(1 2 3)) :parens) "normal tuple")
(assert (= (tuple/type [1 2 3]) :parens) "normal tuple 1")
(assert (= (tuple/type '[1 2 3]) :brackets) "bracketed tuple 2")
(assert (= (tuple/type (-> '(1 2 3) marshal unmarshal)) :parens)
        "normal tuple marshalled/unmarshalled")
(assert (= (tuple/type (-> '[1 2 3] marshal unmarshal)) :brackets)
        "normal tuple marshalled/unmarshalled")

# Bracket tuple issue - 340a6c4

(let [do 3]
  (assert (= [3 1 2 3] [do 1 2 3]) "bracket tuples are never special forms"))
(assert (= ~(,defn 1 2 3) [defn 1 2 3]) "bracket tuples are never macros")
(assert (= ~(,+ 1 2 3) [+ 1 2 3]) "bracket tuples are never function calls")

# Quasiquote bracketed tuples - e239980da
(assert (= (tuple/type ~[1 2 3]) (tuple/type '[1 2 3]))
        "quasiquote bracket tuples")

# Make sure Carriage Returns don't end up in doc strings - e528b86

(assert (not (string/find "\r"
                          (get ((fiber/getenv (fiber/current)) 'cond)
                               :doc ""))) "no \\r in doc strings")

# Detaching closure over non resumable fiber - issue #317
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

# 5c364e0
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

# Inline 3 argument get - a1ea62a
(assert (= 10 (do (var a 10) (set a (get '{} :a a)))) "inline get 1")

#
# Longstring indentation - 7aa4241
#

(defn reindent
  "Reindent the contents of a longstring as the Janet parser would.
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

# Struct prototypes - 4d983e5
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

# missing symbols - issue #914

(defn lookup-symbol [sym] (defglobal sym 10) (dyn sym))

(setdyn :missing-symbol lookup-symbol)

(assert (= (eval-string "(+ a 5)") 15) "lookup missing symbol")

(setdyn :missing-symbol nil)
(setdyn 'a nil)

(assert-error "compile error" (eval-string "(+ a 5)"))

(assert-error
  "table rawget regression"
  (table/new -1))

# Named arguments - 87fc339
(defn named-arguments
  [&named bob sally joe]
  (+ bob sally joe))

(assert (= 15 (named-arguments :bob 3 :sally 5 :joe 7)) "named arguments 1")

# a117252
(defn named-opt-arguments
  [&opt x &named a b c]
  (+ x a b c))

(assert (= 10 (named-opt-arguments 1 :a 2 :b 3 :c 4)) "named arguments 2")

# dacbe29
(def f (asm (disasm (fn [x] (fn [y] (+ x y))))))
(assert (= ((f 10) 37) 47) "asm environment tables")

# First commit removing the integer number type - 6b95326d7
(assert (= 400 (math/sqrt 160000)) "sqrt(160000)=400")

# 88813c4
(assert (deep= (in (disasm (defn a [] (def x 10) x)) :symbolmap)
               @[[0 3 0 'a] [1 3 1 'x]])
        "symbolmap when *debug* is true")

(defn a [arg]
  (def x 10)
  (do
    (def y 20)
    (def z 30)
    (+ x y z)))
(def symbolslots (in (disasm a) :symbolslots))
(def f (asm (disasm a)))
(assert (deep= (in (disasm f) :symbolslots)
               symbolslots)
        "symbolslots survive disasm/asm")

(comment
  (setdyn *debug* true)
  (setdyn :pretty-format "%.40M")
  (def f (fn [x] (fn [y] (+ x y))))
  (assert (deep= (map last (in (disasm (f 10)) :symbolmap))
                 @['x 'y])
          "symbolmap upvalues"))

(assert (deep= (in (disasm (defn a [arg]
                             (def x 10)
                             (do
                               (def y 20)
                               (def z 30)
                               (+ x y z)))) :symbolmap)
               @[[0 7 0 'arg]
                 [0 7 1 'a]
                 [1 7 2 'x]
                 [2 7 3 'y]
                 [3 7 4 'z]])
        "arg & inner symbolmap")

(end-suite)

