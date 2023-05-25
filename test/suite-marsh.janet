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

# More marshalling code

(defn check-image
  "Run a marshaling test using the make-image and load-image functions."
  [x msg]
  (def im (make-image x))
  # (printf "\nimage-hash: %d" (-> im string hash))
  (assert-no-error msg (load-image im)))

(check-image (fn [] (fn [] 1)) "marshal nested functions")
(check-image (fiber/new (fn [] (fn [] 1)))
             "marshal nested functions in fiber")
(check-image (fiber/new (fn [] (fiber/new (fn [] 1))))
             "marshal nested fibers")

(def issue-53-x
  (fiber/new
    (fn []
      (var y (fiber/new (fn [] (print "1") (yield) (print "2")))))))

(check-image issue-53-x "issue 53 regression")

(end-suite)

