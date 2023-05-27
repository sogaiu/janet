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

# More fiber semantics
# 0fd9224e4

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
(assert (= (fiber/status myfiber) :dead)
        "fiber properly dies from debug state")

# Fix yields inside nested fibers
# 909c906
(def yielder
  (coro
    (defer (yield :end)
      (repeat 5 (yield :item)))))
(def items (seq [x :in yielder] x))
(assert (deep= @[:item :item :item :item :item :end] items)
        "yield within nested fibers")

# Calling non functions
# b9c0fc820

(assert (= 1 ({:ok 1} :ok)) "calling struct")
(assert (= 2 (@{:ok 2} :ok)) "calling table")
(assert (= :bad (try ((identity @{:ok 2}) :ok :no) ([err] :bad)))
        "calling table too many arguments")
(assert (= :bad (try ((identity :ok) @{:ok 2} :no) ([err] :bad)))
        "calling keyword too many arguments")
(assert (= :oops (try ((+ 2 -1) 1) ([err] :oops)))
        "calling number fails")

# Method test
# d5bab7262

(def Dog @{:bark (fn bark [self what]
                   (string (self :name) " says " what "!"))})
(defn make-dog
  [name]
  (table/setproto @{:name name} Dog))

(assert (= "fido" ((make-dog "fido") :name)) "oo 1")
(def spot (make-dog "spot"))
(assert (= "spot says hi!" (:bark spot "hi")) "oo 2")

# Negative tests
# 67f26b7d7

(assert-error "+ check types" (+ 1 ()))
(assert-error "- check types" (- 1 ()))
(assert-error "* check types" (* 1 ()))
(assert-error "/ check types" (/ 1 ()))
(assert-error "band check types" (band 1 ()))
(assert-error "bor check types" (bor 1 ()))
(assert-error "bxor check types" (bxor 1 ()))
(assert-error "bnot check types" (bnot ()))

# Comparisons
# 10dcbc639
(assert (> 1e23 100) "less than immediate 1")
(assert (> 1e23 1000) "less than immediate 2")
(assert (< 100 1e23) "greater than immediate 1")
(assert (< 1000 1e23) "greater than immediate 2")

# Regression #638
# c68264802
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

(end-suite)

