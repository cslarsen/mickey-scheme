(import (scheme char)
        (test unit-test)
        (only (scheme write) display))

;; char-whitespace
(testq (char-whitespace? #\a) #f)
(testq (char-whitespace? #\b) #f)
(testq (char-whitespace? #\c) #f)
; TODO: Test for #\tab and friends

;; other char tests
(testq (char-upcase #\a) #\A)
(testq (char-upcase #\A) #\A)
(testq (char-upcase #\h) #\H)
(testq (char-upcase #\z) #\Z)
(testq (char-downcase #\A) #\a)
(testq (char-downcase #\a) #\a)
(testq (char-downcase #\Z) #\z)

;; The following two string-map tests are from R7RS draft 6:
;;
(testq (string-map char-foldcase "AbdEgH") "abdegh")
;;
(testq
  (string-map
    (lambda (c)
      (integer->char (+ 1 (char->integer c))))
        "HAL") "IBM")
;;
(testq
  (string-map
    (lambda (c k)
      ((if (eqv? k #\u) char-upcase char-downcase)
        c))
    "studlycaps xxx"
    "ululululul") "StUdLyCaPs")

;; Taken from R7RS draft 6
;;
(test
  (let ((v '()))
    (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
        "abcde")
        v) '(101 100 99 98 97))

;; N input strings requires N parameters to the lambda:
(test
  (let ((v '()))
    (string-for-each
      (lambda (one two three)
        (set! v (cons three (cons one v))))
      "ab" "cd" "efg") v)
  '(f b e a))

;; string comparison
(testq (string-ci=? "foo" "FoO") #t)
(testq (string-ci=? "foo" "foo") #t)
(testq (string-ci=? "FOO" "FOO") #t)
(testq (string-ci=? "Foo" "FOo") #t)
(testq (string-ci=? "Foo" "FOoz") #f)
(testq (string-ci=? "Foo" "oOo") #f)
(testq (string-ci=? "foo " "foo") #f)
;;
(testq (string-ci<? "AAA" "BBB") #t)
(testq (string-ci<? "aaa" "BBB") #t)
(testq (string-ci<? "AAA" "bbb") #t)
(testq (string-ci<? "aaa" "bbb") #t)
(testq (string-ci<? "AaA" "bBb") #t)
(testq (string-ci<? "AaA" "AaA") #f)
(testq (string-ci<? "aaA" "AAa") #f)
;;
(testq (string-ci>? "AAA" "aAa") #f)
(testq (string-ci>? "AAA" "AAA") #f)
(testq (string-ci>? "AAA" "BBB") #f)
(testq (string-ci>? "aaa" "BBB") #f)
(testq (string-ci>? "AAA" "bbb") #f)
(testq (string-ci>? "aaa" "bbb") #f)
(testq (string-ci>? "AaA" "bBb") #f)
(testq (string-ci>? "AaA" "AaA") #f)
(testq (string-ci>? "aaA" "AAa") #f)
;;
(testq (string-ci>? "aAa" "AAA") #f)
(testq (string-ci>? "AAA" "AAA") #f)
(testq (string-ci>? "BBB" "AAA") #t)
(testq (string-ci>? "BBB" "aaa") #t)
(testq (string-ci>? "bbb" "AAA") #t)
(testq (string-ci>? "bbb" "aaa") #t)
(testq (string-ci>? "BbB" "aAa") #t)
(testq (string-ci>? "BbB" "BbB") #f)
(testq (string-ci>=? "BbB" "BbB") #t)
(testq (string-ci>? "aaA" "AAa") #f)
;;
(testq (string-ci>=? "aaa" "bbb") #f)
(testq (string-ci>=? "AaA" "bBb") #f)
(testq (string-ci>=? "bBb" "AaA") #t)
(testq (string-ci>=? "AaA" "AaA") #t)
(testq (string-ci>=? "aaA" "AAa") #t)
;;
(testq (string-ci<=? "aaa" "bbb") #t)
(testq (string-ci<=? "aaa" "aAa") #t)
(testq (string-ci<=? "AaA" "bBb") #t)
(testq (string-ci<=? "AaA" "AaA") #t)
(testq (string-ci<=? "aaA" "AAa") #t)

;; Char functions
(testq (char->integer #\a) 97)
(testq (char->integer #\b) 98)
(testq (char->integer #\A) 65)
(testq (char-alphabetic? #\a) #t)
(testq (char-alphabetic? #\A) #t)
(testq (char-alphabetic? #\2) #f)
(testq (char-alphabetic? #\8) #f)
(testq (char-lower-case? #\8) #f)
(testq (char-lower-case? #\Z) #f)
(testq (char-lower-case? #\A) #f)
(testq (char-lower-case? #\H) #f)
(testq (char-lower-case? #\z) #t)
(testq (char-lower-case? #\a) #t)
(testq (char-lower-case? #\h) #t)
(testq (char-upper-case? #\8) #f)
(testq (char-upper-case? #\Z) #t)
(testq (char-upper-case? #\A) #t)
(testq (char-upper-case? #\H) #t)
(testq (char-upper-case? #\z) #f)
(testq (char-upper-case? #\a) #f)
(testq (char-upper-case? #\h) #f)
(testq (char-numeric? #\8) #t)
(testq (char-numeric? #\Z) #f)
(testq (char-numeric? #\A) #f)
(testq (char-numeric? #\a) #f)
(testq (char-numeric? #\0) #t)
(testq (char-numeric? #\1) #t)
(testq (char-numeric? #\9) #t)
(testq (digit-value #\0) 0)
(testq (digit-value #\1) 1)
(testq (digit-value #\2) 2)
(testq (digit-value #\3) 3)
(testq (digit-value #\4) 4)
(testq (digit-value #\5) 5)
(testq (digit-value #\6) 6)
(testq (digit-value #\7) 7)
(testq (digit-value #\8) 8)
(testq (digit-value #\9) 9)
(testq (digit-value #\a) #f)
(testq (digit-value #\z) #f)
(testq (digit-value #\x34) 4)
(testq (digit-value #\x35) 5)
(testq (char<=? #\9 #\0) #f)
(testq (char<=? #\3 #\5) #t)
(testq (char<=? #\a #\z) #t)
(testq (char<=? #\a #\a) #t)
(testq (char<=? #\h #\e) #f)
(testq (char<=? #\r #\w) #t)
(testq (char>=? #\9 #\0) #t)
(testq (char>=? #\3 #\5) #f)
(testq (char>=? #\a #\z) #f)
(testq (char>=? #\a #\a) #t)
(testq (char>=? #\h #\e) #t)
(testq (char>=? #\r #\w) #f)
(testq (char>? #\9 #\0) #t)
(testq (char>? #\3 #\5) #f)
(testq (char>? #\a #\a) #f)
(testq (char>? #\h #\e) #t)
(testq (char>? #\r #\w) #f)
(testq (char=? #\9 #\0) #f)
(testq (char=? #\3 #\5) #f)
(testq (char=? #\a #\a) #t)
(testq (char=? #\4 #\4) #t)
(testq (char=? #\h #\e) #f)
(testq (char=? #\r #\w) #f)

;; case-insensitive character predicates
(testq (char-ci=? #\A #\a) #t)
(testq (char-ci=? #\A #\A) #t)
(testq (char-ci=? #\a #\A) #t)
(testq (char-ci=? #\a #\a) #t)
(testq (char-ci=? #\a #\b) #f)
(testq (char-ci=? #\a #\b) #f)
(testq (char-ci=? #\A #\B) #f)
(testq (char-ci=? #\A #\b) #f)

(tap-results)
