(import (scheme load)
        (scheme cxr)
        (scheme base)
        (scheme write)
        (scheme char)
        (scheme math)
        (scheme char)
        (mickey environment)
        (test unit-test))

(display "Loading complex.scm\n")
(load "complex.scm")

(display "Tests\n")
(testq (+ 1 2 3 4) 10)
(testv (+ 1 2 3 4) 10)
(test (+ 1 2 3 4) 10)
(testq (* 1 2 3 4) 24)
(testq (- 1 2 3 4) -8)
(testq (string-append "a" "b" "b" "a") "abba")
(testq (number->string 12345) "12345")
(test (list 1 2 (list 3 4)) (list 1 2 (list 3 4)))
(testq (apply + (list 1 2 3)) 6)
(testq (apply + (quote (1 2 3))) 6)
(testq (apply + (list 1 2 3 (* 5 6))) 36)
(testq (apply + (list 1 2 3 (* 5 5))) 31)
(testq (complex->string
         (*complex (make-complex 2 3)
           (make-complex 5 7))) "-11 + 29i")
(testq (- 0.5 -1.5) 2)
(testq (- 0.5 -10.5) 11)
(testq (- 1 2 3) -4)
(testq (- 1) -1)
(testq (- 2) -2)
(testq (- -2) 2)

(testq (number? 'a) #f)
(testq (number? +) #f)
(testq (number? 1) #t)
(testq (number? 2) #t)
(testq (number? 1.0) #t)
(testq (number? 0) #t)
(testq (number? 0.0) #t)
(testq (number? 0.000) #t)
(testq (number? -0.000) #t)
(testq (number? -1.000) #t)
(testq (number? -1.234) #t)
(testq (number? 1.234) #t)
(testq (number? 1234) #t)

;; Rational numbers
(testq (rational? 1) #t)
(testq (rational? 1.0) #t)
(testq (rational? 2.0) #t)
(testq (rational? 2.1) #t)
(testq (rational? -10) #t)
(testq (rational? 0) #t)
(testq (rational? 10/20) #t)
(testq (= 10/20 1/2) #t)
(testq (= 0.5 1/2) #t)
(testq (= 0.501 1/2) #f)
(testq (= 0.6 1/2) #f)
(testq (= 0.51 1/2) #f)
(testq 1000/200 5)

;; Number prefices
(testq #xFF 255)
(testq #xFFF 4095)
(testq #b11 3)
(testq #o77 63)
(testq #d123 123)
;; ... with exactness
(test #e1.1 11/10)
(test #e1.2 6/5)

;; pair?, list? and dot notation
(test '(1 . 2) (cons 1 2))
(testq (pair? '(1 . 3)) #t)
(testq (pair? (cons 1 3)) #t)
(testq (list? '(1 . 3)) #f)
(testq (list? (cons 1 3)) #f)
(testq (pair? (list 1 2)) #t)
(testq (list? (list 1 2)) #t)
(testq (pair? '()) #f)
(testq (list? '()) #t)
(testq (list? 123) #f)
(testq (pair? 123) #f)
(testq (+ 1 . (2 . (3 . ()))) 6)
(testq (list? '(1 . 2)) #f)

;; (if t a1 a2)
(testq (if (> 4 2) 11 22) 11)
(testq (if (> 3 2) 11 22) 11)
(testq (if (> 2 2) 11 22) 22)
(testq (if (> 1 2) 11 22) 22)

;; (if t a1)
(testq (if (> 3 2) 11) 11)
(testq (if (< 1 2) 11) 11)

;; (and)
(testq (and (= 2 2) (> 2 1)) #t)
(testq (and (= 2 2) (< 2 1)) #f)
(test (and 1 2 'c '(f g)) '(f g))
(testq (and) #t)
(testq (and 1) 1)
(testq (and 0) 0)
(testq (and 0 #f) #f)
(testq (and 1 #f 2) #f)
(test (and 1 2 (quote c) (quote (f g))) '(f g))

;; (or)
(testq (or (= 2 2) (> 2 1)) #t)
(testq (or (= 2 2) (< 2 1)) #t)
(testq (or #f #f #f) #f)
(testq (or #t #f #f) #t)
(testq (or #f #t #f) #t)
(testq (or #f #f #t) #t)
(test (or (memq 'b '(a b c)) (/ 3 0)) '(b c))
(testq (or) #f)
(testq (or #t (/ 1 0)) #t)
(testq (or 10 (/ 1 0)) 10)
(test (or (quote (a b c)) (/ 1 0)) '(a b c))

;; (lambda)
(test ((lambda x x) 3 4 5 6) '(3 4 5 6))
(test ((lambda (x y . z) z) 3 4 5 6) '(5 6))

;; (reverse ...)
(test (reverse (list 1 2 3 4)) (list 4 3 2 1))
(test (reverse (list 1 2 3)) (list 3 2 1))
(test (reverse (list 1 2)) (list 2 1))
(test (reverse (list 1)) (list 1))

;; (abs <int>)
(testq (abs -2) 2)
(testq (abs -1) 1)
(testq (abs 0) 0)
(testq (abs 1) 1)
(testq (abs 2) 2)

;; (abs <float>)
(testq (abs -2.1) 2.1)
(testq (abs -1.1) 1.1)
(testq (abs 0.1) 0.1)
(testq (abs -0.1) 0.1)
(testq (abs 1.1) 1.1)
(testq (abs 2.1) 2.1)

;; car and cdr
(testq (caddr '(1 2 3 4)) 3)
(test (cdddr '(1 2 3 4 5)) '(4 5))
(testq (cadddr '(1 2 3 4 5)) 4)

;; set! and friends
(testq
  (begin
    (set-cdr! (list 1 2) 3)
    123) 123) ;; just check that set-cdr! will accept
               ;; a non-symbol
(test
  (let ((v (list 1 2)))
    (set-cdr! v 3)
    v)
  (cons 1 3))

(testq
  (begin
    (set-car! (list 1 2) 3)
    123) 123) ;; just check that set-car! will accept
               ;; a non-symbol
(test
  (let ((v (list 1 2)))
    (set-car! v 3)
    v)
  '(3 2))

;; assq
(test
  (assq
     (quote three)
     (list (list (quote one) 1)
           (list (quote two) 2)
           (list (quote three) 3)))
   (list (quote three) 3))

(test
  (assq (quote two)
        (list
          (list (quote one) 1)
          (list (quote two) 2)
          (list (quote three) 3)))
  (list (quote two) 2))

(testq
  (assq
    (quote threee)
    (list
      (list (quote one) 1)
      (list (quote two) 2)
      (list (quote three) 3))) #f)

;; even, odd
(testq (even? 0) #t) ;; correct in MIT Scheme
(testq (even? 1) #f)
(testq (even? 2) #t)
(testq (even? 3) #f)
(testq (even? 4) #t)
(testq (even? 9) #f)
(testq (even? 100) #t)
(testq (even? 1000) #t)
(testq (odd? 0) #f) ;; correct in MIT Scheme
(testq (odd? 1) #t)
(testq (odd? 2) #f)
(testq (odd? 3) #t)
(testq (odd? 4) #f)
(testq (odd? 9) #t)
(testq (odd? 100) #f)
(testq (odd? 1000) #f)

;; positive, negative
(testq (negative? 0) #f)
(testq (negative? 1) #f)
(testq (negative? 2) #f)
(testq (negative? 3) #f)
(testq (negative? -1) #t)
(testq (negative? -2) #t)
(testq (negative? -3) #t)
(testq (positive? 0) #f)
(testq (positive? 1) #t)
(testq (positive? 2) #t)
(testq (positive? 3) #t)
(testq (positive? -1) #f)
(testq (positive? -2) #f)
(testq (positive? -3) #f)

;; eqv? tests from R7RS, section 6.1
(testq (eqv? (quote a) (quote a)) #t) ;; (eqv? 'a 'a)
(testq (eqv? (quote a) (quote b)) #f)
(testq (eqv? 2 2) #t)
(testq (eqv? 2 1) #f)
(testq (eqv? (list) (list)) #t) ;; (eqv? '() '()
(testq (eqv? 100000000 100000000) #t)
(testq (eqv? (cons 1 2) (cons 1 2)) #f)
(testq (eqv? (lambda () 1) (lambda () 2)) #f)
(testq (eqv? #f (quote nil)) #f)
(testq (let ((p (lambda (x) x)))
            (eqv? p p)) #t)

;; unspecified tests for eqv
; (eqv? "" "")
; (eqv? '#() '#())
; (eqv? (lambda (x) x) (lambda (x) x))
; (eqv? (lambda (x) x) (lambda (y) y))

;; round
(testq (round 1) 1)
(testq (round 2) 2)
(testq (round 0.9) 1.0)
(testq (round 1.0) 1.0)
(testq (round 1.1) 1.0)
(testq (round 1.2) 1.0)
(testq (round 1.3) 1.0)
(testq (round 1.4) 1.0)
(testq (round 1.5) 2.0)
(testq (round 1.6) 2.0)
(testq (round 2.49) 2.0)
(testq (round 2.50) 3.0) ;; NOTE: Chicken and MIT Scheme reports 2! IEEE-754 magic or?
(testq (round 2.51) 3.0)

;; truncate
(testq (truncate 1.1) 1.0)
(testq (truncate 1.4) 1.0)
(testq (truncate 1.5) 1.0)
(testq (truncate 1.6) 1.0)
(testq (truncate 1.9) 1.0)
(testq (truncate 1) 1)
(testq (truncate 2) 2)

;; min
(testq (min 1 2 3) 1)
(testq (min 4 2 3) 2)
(testq (min 4.2 2 3) 2)
(testq (min 4.2 2.3 3) 2.3)
(testq (min 4.2) 4.2)

;; max
(testq (max 1 2 3) 3)
(testq (max 4 2 3) 4)
(testq (max 4.2 2 3) 4.2)
(testq (max 4.2 2.3 3) 4.2)
(testq (max 4.2) 4.2)

;; expt
(testq (expt 2 0) 1)
(testq (expt 2 1) 2)
(testq (expt 2 2) 4)
(testq (expt 2 3) 8)
(testq (expt 3 3) 27)

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

(test (map + '(1 2) '(3 4) '(4 5)) '(8 11))
(test (map * '(1 2) '(3 4) '(4 5)) '(12 40))

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

;; modulo
(testq (modulo 10 6) 4)
(testq (modulo 10 5) 0)
(testq (modulo 10 4) 2)
(testq (modulo 10 3) 1)
(testq (modulo 10 2) 0)
(testq (modulo 10 1) 0)
; TODO: Test negative modulo, (modulo 10 -3)

;; integer?
(testq (integer? 1) #t)
(testq (integer? 1.0) #t)
(testq (integer? 1.1) #f)
(testq (integer? (quote b)) #f)

;; char functions
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

;; conversion
(testq (integer->char 65) (quote #\A))
(testq (integer->char 97) (quote #\a))
(testq (list->string (list #\a #\b #\c)) "abc")
(testq (list->string (list #\a #\b #\b #\A)) "abbA")
(test (list-tail (list 1 2 3) 0) (list 1 2 3))
(test (list-tail (list 1 2 3) 1) (list 2 3))
(test (list-tail (list 1 2 3) 2) (list 3))
(test (list-tail (list 1 2 3) 3) (list))

;; member
(testq (member 10 (list 1 2 3)) #f)
(test (member 10 (list 10 20 30)) (list 10 20 30))
(test (member 20 (list 10 20 30)) (list 20 30))
(test (member 30 (list 10 20 30)) (list 30))
(testq (member 40 (list 10 20 30)) #f)
(test (member 20
               (list 10 20 30
                     (quote bee)
                     (quote cee)))
       (list 20 30 (quote bee) (quote cee)))


;; memv (TODO: insert eqv? specific check)
(testq (memv 10 (list 1 2 3)) #f)
(test (memv 10 (list 10 20 30)) (list 10 20 30))
(test (memv 20 (list 10 20 30)) (list 20 30))
(test (memv 30 (list 10 20 30)) (list 30))
(testq (memv 40 (list 10 20 30)) #f)
(test (memv 20 (list 10 20 30
                      (quote bee)
                      (quote cee)))
       (list 20 30 (quote bee) (quote cee)))

;; memq (TODO: insert eq? specific check)
(testq (memq 10 (list 1 2 3)) #f)
(test (memq 10 (list 10 20 30)) (list 10 20 30))
(test (memq 20 (list 10 20 30)) (list 20 30))
(test (memq 30 (list 10 20 30)) (list 30))
(testq (memq 40 (list 10 20 30)) #f)
(test (memq 20 (list 10 20 30
                      (quote bee) (quote cee)))
       (list 20 30 (quote bee) (quote cee)))

;; gcd, lcm
(testq (gcd) 0)
(testq (gcd 10) 10)
(testq (gcd -10) 10)
(testq (gcd 10 2) 2)
(testq (gcd 10 3) 1)
(testq (gcd 10 5) 5)
(testq (gcd 5 10) 5)
(testq (gcd 32 -36) 4)
(testq (gcd -32 36) 4)
(testq (gcd -32 -36) 4)
(testq (gcd 4 6 8 10) 2)
(testq (gcd 4 6 8 12) 2)
(testq (gcd 40 24) 8)
(testq (gcd 1230 4560) 30)
(testq (lcm) 1)
(testq (lcm 10) 10)
(testq (lcm -10) 10)
(testq (lcm 10 10) 10)
(testq (lcm 4 6) 12)
(testq (lcm 6 4) 12)
(testq (lcm 2 4 6) 12)
(testq (lcm 2 4 6 8) 24)
(testq (lcm 2 4 6 -8) 24)
(testq (lcm 2 -4 6 -8) 24)
(testq (lcm 201 202 203) 8242206)
(testq (lcm 201 202 203 204) 280235004)

;; list-ref
(testq (list-ref (list 1 2 3) 2) 3)
(testq (list-ref (list 1 2 3) 1) 2)
(testq (list-ref (list 1 2 3) 0) 1)
(testq (list-ref (list 1 2 3 4) 0) 1)
(testq (list-ref (list 1 2 3 4) 3) 4)

;; string conversion
(test (string->list "hey") (list #\h #\e #\y))
(testq (string->symbol "hey") (quote hey))
(testq (string->number "abba") #f)
(testq (string->number "123") 123)
(testq (string->number "456") 456)
(testq (string->number "1.2") 1.2)
(testq (string->number "1.5") 1.5)
(testq (string-length "abba") 4)
(testq (string-length "abb") 3)
(testq (string-length "ab") 2)
(testq (string-length "a") 1)
(testq (string-length "") 0)
(testq (substring "Hello!" 0 0) "")
(testq (substring "Hello!" 0 1) "H")
(testq (substring "Hello!" 0 3) "Hel")
(testq (substring "Hello!" 1 3) "ell")
(testq (substring "Hello!" 1 3) "ell")
(testq (substring "Hello!" 1 4) "ello")
(testq (substring "Hello!" 2 4) "llo!")
(testq (string=? "hey" "hey") #t)
(testq (string=? "hey" "heya") #f)
(testq (string<=? "hey" "heya") #t)
(testq (string-ref "hey" 0) #\h)
(testq (string-ref "hey" 1) #\e)
(testq (string-ref "hey" 2) #\y)

;; append tests
(testq (append (list) 1) 1)
(test (append (list) (list 1 2)) (list 1 2))
(test (append (list) (list 1 2)) (list 1 2))
(test (append (list 1)) (list 1))
(test (append (list 1)) (list 1))
(test (append (list 1) 2) (cons 1 2))
(test (append (list 1) (list 3)) (list 1 3))
(test (append (list 1) (list 3 4)) (list 1 3 4))
(test (append (list 1) (list 3 4) 5) (cons 1 (cons 3 (cons 4 5))))
(test (append
         (append
           (list 1 2 3)
           (list 4))
         (list 5 6)
           7)
       (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))

;; Regression test: append should not mutate
(test
  (begin
    (define a (list 1))
    (append a 2)
    (append a 3)
    a) (list 1))

;; Quoting
(test 'hey (quote hey))
(test 'hey 'hey)
(test ''hey ''hey)
(test (append '(1 2 3) 4) (cons 1 (cons 2 (cons 3 4))))
(testq (apply + '(1 2 3)) 6)
(testq (apply + '(1 2 3)) 6) ; is double-quote ok?
;
(test (quasiquote (1 2 (unquote (+ 3 4)))) '(1 2 7)) ; this test should work nicely (TODO)
(test (quasiquote (1 2 (unquote (+ 3 4)))) (list 1 2 7)) ; this test should work nicely (TODO)
(test `(1 2 ,(+ 3 4) 3 y) '(1 2 7 3 y))
;
; Quasiquoting with unquote-splicing ",@"
;(testq (quote (define foo '(11 22 33)) `(1 2 3 foo ,foo ,@foo)) (quote (1 2 3 foo (11 22 33) 11 22 33)))
(test `(1 2 ,@(list 3 4 5) 6 7) (list 1 2 3 4 5 6 7))

;; Math module
(testq (ceiling 3.0) 3.0)
(testq (ceiling 3.1) 4.0)
(testq (ceiling 3.4) 4.0)
(testq (ceiling 3.5) 4.0)
(testq (ceiling 3.6) 4.0)
(testq (floor 3.0) 3.0)
(testq (floor 3.1) 3.0)
(testq (floor 3.4) 3.0)
(testq (floor 3.5) 3.0)
(testq (floor 3.6) 3.0)
(testq (floor 3.9) 3.0)
(testq (floor 3.999) 3.0)
(test (sqrt 3.999) 1.99975)
(testq (sqrt 4.0) 2.0)
(test (sqrt -4.0) (sqrt -1)) ; nan
(test (exp 1) 2.71828)
(test (exp 2) 7.38906)
(test (atan 45.0) 1.54858)
(test (atan 12.3) 1.48967)
(test (acos 1.0) 0.0)
(test (acos 0.5) 1.0472)
(test (asin 1.0) 1.5708)
(test (asin 0.5) 0.523599)
(test (tan 0.5) 0.546302)
(test (cos 0.5) 0.877583)
(test (sin (* 0.5 3.1415926535)) 1.0)
(test (log 256) 5.54518)
(test (/ (log 256) (log 2)) 8)
(test (/ (log 1024) (log 2)) 10)

;; Number system
(testq (real? 3) #t) ; from r7rs draft

;; Characters
(testq (string-length (make-string 0)) 0)
(testq (string-length (make-string 1)) 1)
(testq (string-length (make-string 2)) 2)
(testq (string-length (make-string 42)) 42)
(testq (string-length (make-string 42 #\a)) 42)
(testq (string-length (make-string 42 #\0)) 42)
(test (make-string 1 #\x28) "(")
(test (make-string 2 #\x28) "((")
(test (make-string 3 #\x28) "(((")
(test (make-string 3 #\x29) ")))")
(test (make-string 5 #\a) "aaaaa")
(test (make-string 5 #\A) "AAAAA")
(test (make-string 7 #\space) "       ")

;; TODO/BUG: Here's another one.  When parsing "#\newline", it is converted
;; to char and then displayed by sprint() / print() as a newline, instead of
;; the literal "#\newline". Fix that.
(testq (string-length (make-string 9 #\newline)) 9)
(testq (string-length (make-string 7 #\tab)) 7)
(testq (string-length (make-string 3 #\x1)) 3)

;; The following two tests are BUGs in our implementation, because we use zero
;; terminated strings internally for the STRING type, and therefore
;; cannot count using strlen().  Either switch to std::string or reimplement
;; STRING.
(testq (string-length (make-string 3 #\x0)) 3) ; <- TODO/BUG: Fix code!
(testq (string-length (make-string 3 #\null)) 3) ; <- TODO/BUG: Fix code!

;; Comment-out datum
(testq (+ 1 #; 2 3) 4)
(testq (+ 1 #;2 3) 4)

;; Quasiquote
(test `(1 2 ,(+ 3 4)) '(1 2 7))
(testq '() (list))
(testq '() '())
(testq (list) '())
(testq (list) '())

;; Vectors
(testq (vector-length (vector 1 2 3)) 3)
(testq (vector-length (vector)) 0)
(testq (vector-length (vector 1 2 foo)) 3) ; self-evaluating elements
(testq
  (vector-length
    (vector 1 2 (define (v foo) (* foo foo)) 100)) 4)

(testq (list? (cons 1 2)) #f)
(testq (list? (quote (a . b))) #f)
(testq (pair? (cons 1 2)) #t)
(testq (pair? (quote (a . b))) #t)
(testq (length (list '() '())) 2)

; to fix below bugs, tokenizer should must return 3 tokens for "a""b""c"
(testq (length '(#;"a""b""c")) 2)
(testq (length '("a"#;"b""c""d""e")) 4)

; to fix, enable support for \x<hex-number> escapes in string parser
(testq (string-length "a\x42;c") 3)
(testq "a\x42;c" "aBc")

(test (map (lambda (x) (* x x)) '(0 1 2 3 4 5)) '(0 1 4 9 16 25))

;; Exactness
(testq (exact? 3.4) #f)
(testq (exact? 10.0) #f)
(testq (exact? (* 2 2)) #t)
(testq (exact? (* 2 2.0)) #f)
(testq (exact? (+ 1 2 3)) #t)
(testq (exact? (+ 1 2 3 (* 10 20))) #t)
(testq (exact? (+ 1 2 3 (* 10 20 4))) #t)
(testq (exact? (+ 1 2 3 (* 10 20 4 1))) #t)
(testq (exact? (+ 1 2 3 (* 10 20 4 1.0))) #f)
(testq (exact? (exact 1)) #t)
(testq (exact? (exact 1.1)) #t)
(testq (exact? (exact -1.1)) #t)
(testq (exact? (exact -1)) #t)
(testq (exact? (exact 0)) #t)
(testq (exact? #e123) #t)
(testq (inexact? #e123) #f)
(testq (exact? #i123) #f)
(testq (inexact? #i123) #t)
(testq (exact? #e123.456) #t)
(testq (inexact? #e123.456) #f)
(testq (inexact? #i123.456) #t)
(testq (exact? #i123.456) #f)
(test (exact 10) 10)
(test (exact 10.2) 51/5)
(test (exact 0.1) 1/10)
(test (exact 0.2) 2/10)
(test (exact 0.02) 2/100)
(test (exact 0.002) 2/1000)
(test (exact 1.1) 11/10)
(test (exact 1.2) 6/5)
(test (equal? (inexact 10/2) (inexact 5)) #t)
(test (inexact 10/2) (inexact 5))
(test (inexact 10/2) 5.0)
(test (inexact 1/2) 0.5)
(test (inexact 1/2) .5)
(test (inexact? (inexact 5)) #t)
(test (inexact 10/3) 3.33333)
(test (exact 123.456) 15432/125)
(test (exact .1) 1/10)
(test (exact .01) 1/100)
(test (exact .001) 1/1000)
(test (exact .0001) 1/10000)
(test (exact .00001) 1/100000)
(test (exact .000001) 1/1000000)
(test (exact .0000001) 1/10000000)
(test (exact .0000002) 1/5000000)
(test (exact .0000003) 3/10000000)

(display "\nResults\n")
(display (string-append
  "  Total: " (number->string (cadr (assq 'total (result))))
  "   Good: " (number->string (cadr (assq 'good (result))))
  "   Fail: " (number->string (cadr (assq 'fail (result)))) "\n"))
