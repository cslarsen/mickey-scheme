(import (scheme char)
        (test unit-test)
        (only (scheme write) display))

(define name "(scheme char)")

;; char-whitespace
(testq name (char-whitespace? #\a) #f)
(testq name (char-whitespace? #\b) #f)
(testq name (char-whitespace? #\c) #f)
; TODO: Test for #\tab and friends

;; other char tests
(testq name (char-upcase #\a) #\A)
(testq name (char-upcase #\A) #\A)
(testq name (char-upcase #\h) #\H)
(testq name (char-upcase #\z) #\Z)
(testq name (char-downcase #\A) #\a)
(testq name (char-downcase #\a) #\a)
(testq name (char-downcase #\Z) #\z)

;; The following two string-map tests are from R7RS draft 6:
;;
(testq name (string-map char-foldcase "AbdEgH") "abdegh")
;;
(testq name
  (string-map
    (lambda (c)
      (integer->char (+ 1 (char->integer c))))
        "HAL") "IBM")
;;
(testq name
  (string-map
    (lambda (c k)
      ((if (eqv? k #\u) char-upcase char-downcase)
        c))
    "studlycaps xxx"
    "ululululul") "StUdLyCaPs")

;; Taken from R7RS draft 6
;;
(test name
  (let ((v '()))
    (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
        "abcde")
        v) '(101 100 99 98 97))

;; N input strings requires N parameters to the lambda:
(test name
  (let ((v '()))
    (string-for-each
      (lambda (one two three)
        (set! v (cons three (cons one v))))
      "ab" "cd" "efg") v)
  '(f b e a))

;; string comparison
(testq name (string-ci=? "foo" "FoO") #t)
(testq name (string-ci=? "foo" "foo") #t)
(testq name (string-ci=? "FOO" "FOO") #t)
(testq name (string-ci=? "Foo" "FOo") #t)
(testq name (string-ci=? "Foo" "FOoz") #f)
(testq name (string-ci=? "Foo" "oOo") #f)
(testq name (string-ci=? "foo " "foo") #f)
;;
(testq name (string-ci<? "AAA" "BBB") #t)
(testq name (string-ci<? "aaa" "BBB") #t)
(testq name (string-ci<? "AAA" "bbb") #t)
(testq name (string-ci<? "aaa" "bbb") #t)
(testq name (string-ci<? "AaA" "bBb") #t)
(testq name (string-ci<? "AaA" "AaA") #f)
(testq name (string-ci<? "aaA" "AAa") #f)
;;
(testq name (string-ci>? "AAA" "aAa") #f)
(testq name (string-ci>? "AAA" "AAA") #f)
(testq name (string-ci>? "AAA" "BBB") #f)
(testq name (string-ci>? "aaa" "BBB") #f)
(testq name (string-ci>? "AAA" "bbb") #f)
(testq name (string-ci>? "aaa" "bbb") #f)
(testq name (string-ci>? "AaA" "bBb") #f)
(testq name (string-ci>? "AaA" "AaA") #f)
(testq name (string-ci>? "aaA" "AAa") #f)
;;
(testq name (string-ci>? "aAa" "AAA") #f)
(testq name (string-ci>? "AAA" "AAA") #f)
(testq name (string-ci>? "BBB" "AAA") #t)
(testq name (string-ci>? "BBB" "aaa") #t)
(testq name (string-ci>? "bbb" "AAA") #t)
(testq name (string-ci>? "bbb" "aaa") #t)
(testq name (string-ci>? "BbB" "aAa") #t)
(testq name (string-ci>? "BbB" "BbB") #f)
(testq name (string-ci>=? "BbB" "BbB") #t)
(testq name (string-ci>? "aaA" "AAa") #f)
;;
(testq name (string-ci>=? "aaa" "bbb") #f)
(testq name (string-ci>=? "AaA" "bBb") #f)
(testq name (string-ci>=? "bBb" "AaA") #t)
(testq name (string-ci>=? "AaA" "AaA") #t)
(testq name (string-ci>=? "aaA" "AAa") #t)
;;
(testq name (string-ci<=? "aaa" "bbb") #t)
(testq name (string-ci<=? "aaa" "aAa") #t)
(testq name (string-ci<=? "AaA" "bBb") #t)
(testq name (string-ci<=? "AaA" "AaA") #t)
(testq name (string-ci<=? "aaA" "AAa") #t)

;; Char functions
(testq name (char->integer #\a) 97)
(testq name (char->integer #\b) 98)
(testq name (char->integer #\A) 65)
(testq name (char-alphabetic? #\a) #t)
(testq name (char-alphabetic? #\A) #t)
(testq name (char-alphabetic? #\2) #f)
(testq name (char-alphabetic? #\8) #f)
(testq name (char-lower-case? #\8) #f)
(testq name (char-lower-case? #\Z) #f)
(testq name (char-lower-case? #\A) #f)
(testq name (char-lower-case? #\H) #f)
(testq name (char-lower-case? #\z) #t)
(testq name (char-lower-case? #\a) #t)
(testq name (char-lower-case? #\h) #t)
(testq name (char-upper-case? #\8) #f)
(testq name (char-upper-case? #\Z) #t)
(testq name (char-upper-case? #\A) #t)
(testq name (char-upper-case? #\H) #t)
(testq name (char-upper-case? #\z) #f)
(testq name (char-upper-case? #\a) #f)
(testq name (char-upper-case? #\h) #f)
(testq name (char-numeric? #\8) #t)
(testq name (char-numeric? #\Z) #f)
(testq name (char-numeric? #\A) #f)
(testq name (char-numeric? #\a) #f)
(testq name (char-numeric? #\0) #t)
(testq name (char-numeric? #\1) #t)
(testq name (char-numeric? #\9) #t)
(testq name (digit-value #\0) 0)
(testq name (digit-value #\1) 1)
(testq name (digit-value #\2) 2)
(testq name (digit-value #\3) 3)
(testq name (digit-value #\4) 4)
(testq name (digit-value #\5) 5)
(testq name (digit-value #\6) 6)
(testq name (digit-value #\7) 7)
(testq name (digit-value #\8) 8)
(testq name (digit-value #\9) 9)
(testq name (digit-value #\a) #f)
(testq name (digit-value #\z) #f)
(testq name (digit-value #\x34) 4)
(testq name (digit-value #\x35) 5)
(testq name (char<=? #\9 #\0) #f)
(testq name (char<=? #\3 #\5) #t)
(testq name (char<=? #\a #\z) #t)
(testq name (char<=? #\a #\a) #t)
(testq name (char<=? #\h #\e) #f)
(testq name (char<=? #\r #\w) #t)
(testq name (char>=? #\9 #\0) #t)
(testq name (char>=? #\3 #\5) #f)
(testq name (char>=? #\a #\z) #f)
(testq name (char>=? #\a #\a) #t)
(testq name (char>=? #\h #\e) #t)
(testq name (char>=? #\r #\w) #f)
(testq name (char>? #\9 #\0) #t)
(testq name (char>? #\3 #\5) #f)
(testq name (char>? #\a #\a) #f)
(testq name (char>? #\h #\e) #t)
(testq name (char>? #\r #\w) #f)
(testq name (char=? #\9 #\0) #f)
(testq name (char=? #\3 #\5) #f)
(testq name (char=? #\a #\a) #t)
(testq name (char=? #\4 #\4) #t)
(testq name (char=? #\h #\e) #f)
(testq name (char=? #\r #\w) #f)

;; case-insensitive character predicates
(testq name (char-ci=? #\A #\a) #t)
(testq name (char-ci=? #\A #\A) #t)
(testq name (char-ci=? #\a #\A) #t)
(testq name (char-ci=? #\a #\a) #t)
(testq name (char-ci=? #\a #\b) #f)
(testq name (char-ci=? #\a #\b) #f)
(testq name (char-ci=? #\A #\B) #f)
(testq name (char-ci=? #\A #\b) #f)

