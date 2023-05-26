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

# Peg

(defn check-match
  [pat text should-match]
  (def result (peg/match pat text))
  (assert (= (not should-match) (not result))
          (string "check-match " text)))

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
  (assert (= (string/replace-all "dog" "purple panda" text)
             (0 (peg/match grammar text))) text))

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
            (* `"` (% (any (+ (<- (if-not `"` 1))
                              (* (constant `"`) `""`)))) `"`)
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
(check-match backmatcher-1 (string (string/repeat "x" 73) "y") false)
(check-match backmatcher-1 (string (string/repeat "x" 10000) "y") false)
(check-match backmatcher-1 (string (string/repeat "x" 10000) "y"
                                   (string/repeat "x" 10000)) true)

(def backmatcher-2 '(* '(any "x") "y" (backmatch) -1))

(check-match backmatcher-2 "y" true)
(check-match backmatcher-2 "xyx" true)
(check-match backmatcher-2 "xxxxxxxyxxxxxxx" true)
(check-match backmatcher-2 "xyxx" false)
(check-match backmatcher-2 (string (string/repeat "x" 73) "y") false)
(check-match backmatcher-2 (string (string/repeat "x" 10000) "y") false)
(check-match backmatcher-2 (string (string/repeat "x" 10000) "y"
                                   (string/repeat "x" 10000)) true)

(def longstring-2 '(* '(some "`")
                      (some (if-not (backmatch) 1))
                      (backmatch) -1))

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

# Peg swallowing errors
(assert (try (peg/match ~(/ '1 ,(fn [x] (nil x))) "x") ([err] err))
        "errors should not be swallowed")
(assert (try ((fn [x] (nil x))) ([err] err))
        "errors should not be swallowed 2")

# Check for bad memoization (+ :a) should mean different things in
# different contexts.
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
(check-deep '(uint 8) "\xff\x7f\x00\x00\x00\x00\x00\x00"
            @[(int/u64 0x7fff)])
(check-deep '(int 8) "\xff\x7f\x00\x00\x00\x00\x00\x00"
            @[(int/s64 0x7fff)])
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
(check-deep grammar "<p><em>foobar</em></p>"
            @[{:tag "p" :value @[{:tag "em" :value @["foobar"]}]}])
(check-deep grammar "<p>foobar</p>" @[{:tag "p" :value @["foobar"]}])

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
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF")
                 (set "!$%&*+-./:<?=>@^_|"))
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
                 :close (cmt (* (not (> -1 "`")) (-> :n) '(backmatch :n))
                             ,=)
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

(end-suite)

