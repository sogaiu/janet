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

# 7e46ead2f
(assert (not false) "false literal")
(assert true "true literal")
(assert (not nil) "nil literal")

(assert (= '(1 2 3) (quote (1 2 3)) (tuple 1 2 3)) "quote shorthand")

# Long strings - 7e6342720

(assert (= "hello, world" `hello, world`) "simple long string")
(assert (= "hello, \"world\"" `hello, "world"`)
        "long string with embedded quotes")
(assert (= "hello, \\\\\\ \"world\"" `hello, \\\ "world"`)
        "long string with embedded quotes and backslashes")

# Parser clone - 43520ac67
(def p (parser/new))
(assert (= 7 (parser/consume p "(1 2 3 ")) "parser 1")
(def p2 (parser/clone p))
(parser/consume p2 ") 1 ")
(parser/consume p ") 1 ")
(assert (deep= (parser/status p) (parser/status p2)) "parser 2")
(assert (deep= (parser/state p) (parser/state p2)) "parser 3")

# Parser errors - 976dfc719
(defn parse-error [input]
  (def p (parser/new))
  (parser/consume p input)
  (parser/error p))

# Invalid utf-8 sequences
(assert (not= nil (parse-error @"\xc3\x28")) "reject invalid utf-8 symbol")
(assert (not= nil (parse-error @":\xc3\x28")) "reject invalid utf-8 keyword")

# Parser line and column numbers - 77b79e989
(defn parser-location [input &opt location]
  (def p (parser/new))
  (parser/consume p input)
  (if location
    (parser/where p ;location)
    (parser/where p)))

(assert (= [1 7] (parser-location @"(+ 1 2)")) "parser location 1")
(assert (= [5 7] (parser-location @"(+ 1 2)" [5])) "parser location 2")
(assert (= [10 10] (parser-location @"(+ 1 2)" [10 10])) "parser location 3")

# Issue #861 - should be valgrind clean - 39c6be7cb
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

(end-suite)

