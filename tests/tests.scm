(import (scheme load)
      (scheme cxr)
      (scheme base)
      (scheme write)
      (scheme char)
      (scheme inexact)
      (scheme char)
      (mickey environment)
      (test unit-test))

(load "complex.scm")

(testq "tests.scm" (+ 1 2 3 4) 10)
(testv "tests.scm" (+ 1 2 3 4) 10)
(test "tests.scm" (+ 1 2 3 4) 10)
(testq "tests.scm" (* 1 2 3 4) 24)
(testq "tests.scm" (- 1 2 3 4) -8)
(testq "tests.scm" (string-append "a" "b" "b" "a") "abba")
(testq "tests.scm" (number->string 12345) "12345")
(test "tests.scm" (list 1 2 (list 3 4)) (list 1 2 (list 3 4)))
(testq "tests.scm" (apply + (list 1 2 3)) 6)
(testq "tests.scm" (apply + (quote (1 2 3))) 6)
(testq "tests.scm" (apply + (list 1 2 3 (* 5 6))) 36)
(testq "tests.scm" (apply + (list 1 2 3 (* 5 5))) 31)
(testq "tests.scm" (complex->string
       (*complex (make-complex 2 3)
         (make-complex 5 7))) "-11 + 29i")
(testq "tests.scm" (- 0.5 -1.5) 2.0)
(testq "tests.scm" (- 0.5 -10.5) 11.0)
(testq "tests.scm" (exact? (- 0.5 -1.5)) #f) ; is this really universally true?
(testq "tests.scm" (- 1 2 3) -4)
(testq "tests.scm" (- 1) -1)
(testq "tests.scm" (- 2) -2)
(testq "tests.scm" (- -2) 2)

(testq "tests.scm" (number? 'a) #f)
(testq "tests.scm" (number? +) #f)
(testq "tests.scm" (number? 1) #t)
(testq "tests.scm" (number? 2) #t)
(testq "tests.scm" (number? 1.0) #t)
(testq "tests.scm" (number? 0) #t)
(testq "tests.scm" (number? 0.0) #t)
(testq "tests.scm" (number? 0.000) #t)
(testq "tests.scm" (number? -0.000) #t)
(testq "tests.scm" (number? -1.000) #t)
(testq "tests.scm" (number? -1.234) #t)
(testq "tests.scm" (number? 1.234) #t)
(testq "tests.scm" (number? 1234) #t)

;; Rational numbers
(testq "tests.scm" (rational? 1) #t)
(testq "tests.scm" (rational? 1.0) #t)
(testq "tests.scm" (rational? 2.0) #t)
(testq "tests.scm" (rational? 2.1) #t)
(testq "tests.scm" (rational? -10) #t)
(testq "tests.scm" (rational? 0) #t)
(testq "tests.scm" (rational? 10/20) #t)
(testq "tests.scm" (= 10/20 1/2) #t)
(testq "tests.scm" (= 0.5 1/2) #t)
(testq "tests.scm" (= 0.501 1/2) #f)
(testq "tests.scm" (= 0.6 1/2) #f)
(testq "tests.scm" (= 0.51 1/2) #f)
(testq "tests.scm" 1000/200 5)

;; Rational arithmetic
(test "tests.scm" (* 1/2 1/2) 1/4)
(test "tests.scm" (exact? (* 1/2 1/2)) #t)
(test "tests.scm" (* 1/2 1/2 2) 1/2)
(test "tests.scm" (* 1/2 1/2 2 2) 1)
(test "tests.scm" (exact? (* 1/2 1/2 2 2)) #t)
(test "tests.scm" (exact? (* 1/2 1/2 2 #i2)) #f)
(test "tests.scm" (* 1/2 1/2 2 3) 3/2)
(test "tests.scm" (rational? (* 1/2 1/2 2 3)) #t)
(test "tests.scm" (* 1/2 1/2 2 10) 5)
(test "tests.scm" (integer? (* 1/2 1/2 2 10)) #t)
(test "tests.scm" (exact? (* 1/2 1/2 2 10)) #t)
(test "tests.scm" (inexact? (* 1/2 1/2 2 10)) #f)
(test "tests.scm" (inexact? (* 1/2 1/2 #i2 10)) #t)
(test "tests.scm" (+ 1/2 1/2) 1)
(test "tests.scm" (+ 1/4 1/4) 1/2)
(test "tests.scm" (+ 1/2 0.2) 0.7)
(test "tests.scm" (+ 1/2 1) 3/2)
(testq "tests.scm" (exact? (+ 1/2 1/2)) #t)
(testq "tests.scm" (exact? (+ 1/4 1/4)) #t)
(testq "tests.scm" (exact? (+ 1/2 0.2)) #f)
(testq "tests.scm" (exact? (+ 1/2 1)) #t)

;; Zero predicate
(test "tests.scm" (zero? -1/2) #f)
(test "tests.scm" (zero? -0.2) #f)
(test "tests.scm" (zero? -1) #f)
(test "tests.scm" (zero? 0) #t)
(test "tests.scm" (zero? 0/1) #t)
(test "tests.scm" (zero? 0.0) #t)
(test "tests.scm" (zero? 1) #f)
(test "tests.scm" (zero? 1/2) #f)
(test "tests.scm" (zero? 0.2) #f)

;; Positive and negative infinities

(test "tests.scm" (< -inf.0 0) #t)
(test "tests.scm" (> -inf.0 0) #f)

(test "tests.scm" (> +inf.0 0) #t)
(test "tests.scm" (< +inf.0 0) #f)

(test "tests.scm" (- -inf.0) +inf.0)
(test "tests.scm" (- +inf.0) -inf.0)

(test "tests.scm" (log 0) -inf.0)
(test "tests.scm" (- (log 0)) +inf.0)

(test "tests.scm" (infinite? -inf.0) #t)
(test "tests.scm" (infinite? +inf.0) #t)
(test "tests.scm" (infinite? 123) #f)
(test "tests.scm" (infinite? -123) #f)

; negative infinity
(test "tests.scm" (number->string (/ 10 -inf.0)) "-0")

(test "tests.scm" (+ 1 +nan.0) +nan.0)

;; Number radix / prefix
(testq "tests.scm" #xFF 255)
(testq "tests.scm" #xFFF 4095)
(testq "tests.scm" #b11 3)
(testq "tests.scm" #b01001001100101100000001011010010 1234567890)
(testq "tests.scm" #o77 63)
(testq "tests.scm" #d123 123)
;; ... with exactness
(test "tests.scm" #e1.1 11/10)
(test "tests.scm" #e1.2 6/5)

;; Test exactness
(testq "tests.scm" (exact? 1) #t)
(testq "tests.scm" (exact? 1.1) #f)
(testq "tests.scm" (exact? (+ 1 2 3)) #t)
(testq "tests.scm" (exact? (+ 1 2/3 3)) #t)
(testq "tests.scm" (exact? (* 1 2/3 3)) #t)
(testq "tests.scm" (exact? (* 1 2/3 #i3)) #f)
(testq "tests.scm" (exact? (* #i1 2/3 3)) #f)
(testq "tests.scm" (exact? (/ 8 2)) #t)
(testq "tests.scm" (exact? (/ 10 10)) #t)


;; pair?, list? and dot notation
(test "tests.scm" '(1 . 2) (cons 1 2))
(testq "tests.scm" (pair? '(1 . 3)) #t)
(testq "tests.scm" (pair? (cons 1 3)) #t)
(testq "tests.scm" (list? '(1 . 3)) #f)
(testq "tests.scm" (list? (cons 1 3)) #f)
(testq "tests.scm" (pair? (list 1 2)) #t)
(testq "tests.scm" (list? (list 1 2)) #t)
(testq "tests.scm" (pair? '()) #f)
(testq "tests.scm" (list? '()) #t)
(testq "tests.scm" (list? 123) #f)
(testq "tests.scm" (pair? 123) #f)
(testq "tests.scm" (+ 1 . (2 . (3 . ()))) 6)
(testq "tests.scm" (list? '(1 . 2)) #f)

;; (if t a1 a2)
(testq "tests.scm" (if (> 4 2) 11 22) 11)
(testq "tests.scm" (if (> 3 2) 11 22) 11)
(testq "tests.scm" (if (> 2 2) 11 22) 22)
(testq "tests.scm" (if (> 1 2) 11 22) 22)

;; (if t a1)
(testq "tests.scm" (if (> 3 2) 11) 11)
(testq "tests.scm" (if (< 1 2) 11) 11)

;; (and)
(testq "tests.scm" (and (= 2 2) (> 2 1)) #t)
(testq "tests.scm" (and (= 2 2) (< 2 1)) #f)
(test "tests.scm" (and 1 2 'c '(f g)) '(f g))
(testq "tests.scm" (and) #t)
(testq "tests.scm" (and 1) 1)
(testq "tests.scm" (and 0) 0)
(testq "tests.scm" (and 0 #f) #f)
(testq "tests.scm" (and 1 #f 2) #f)
(test "tests.scm" (and 1 2 (quote c) (quote (f g))) '(f g))

;; (or)
(testq "tests.scm" (or (= 2 2) (> 2 1)) #t)
(testq "tests.scm" (or (= 2 2) (< 2 1)) #t)
(testq "tests.scm" (or #f #f #f) #f)
(testq "tests.scm" (or #t #f #f) #t)
(testq "tests.scm" (or #f #t #f) #t)
(testq "tests.scm" (or #f #f #t) #t)
(test "tests.scm" (or 1 (/ 0)) 1)

;; Equal operator
(testv "tests.scm" (= 1 1 1 1 1) #t)
(testv "tests.scm" (= 1 1 1 1 1.0 1 1.0 1.0 1) #t)
(testv "tests.scm" (= 1 1 2 1 1.0 1 1.0 1.0 1) #f)
(testv "tests.scm" (= 1 1 2 1 1.0 1 1.0 1.01 1) #f)
(testv "tests.scm" (= 1 1 1 1 1.0 1 1.0 1.01 1) #f)
(testv "tests.scm" (= 0.51 0.51 0.51 0.51) #t)
(testv "tests.scm" (= 0.51 0.51 0.52 0.51) #f)
(testv "tests.scm" (= 1/2 1/2 1/2 0.5 10/20 11/22 (* 0.1 5)) #t)

;; Comparison operators
(testv "tests.scm" (< 1 2) #t)
(testv "tests.scm" (< 2 1) #f)
(testv "tests.scm" (< 1 2 3) #t)
(testv "tests.scm" (< -1 2 3) #t)
(testv "tests.scm" (< -2 -1 0 1 2 3) #t)
(testv "tests.scm" (< -2 -1 0 1 2 3 2) #f)
(testv "tests.scm" (< -2 -1 0 1 2 3 4) #t)
(testv "tests.scm" (< -2 -1 -1 1 2 3 4) #f)
(testv "tests.scm" (< -2 -1 -2 1 2 3 4) #f)
(testv "tests.scm" (< -2 -1 1 2 3 4) #t)
(testv "tests.scm" (< -2 -1 +nan.0 2 3 4) #f)

(testv "tests.scm" (> 2 1) #t)
(testv "tests.scm" (> 3 2 1) #t)
(testv "tests.scm" (> 1 2) #f)
(testv "tests.scm" (> 3 2) #t)
(testv "tests.scm" (> 2 3) #f)
(testv "tests.scm" (> 4 3 2 1) #t)
(testv "tests.scm" (> 4 2 0 -1) #t)
(testv "tests.scm" (> -1 -2) #t)
(testv "tests.scm" (> -10 -20) #t)
(testv "tests.scm" (> -10 -20 -30) #t)
(testv "tests.scm" (> -10 +nan.0 -30) #f)
(testv "tests.scm" (> -10 -20 -4) #f)

(testv "tests.scm" (<= 1 2 3 3 4 4 10 12 14 14) #t)
(testv "tests.scm" (<= 1 2 3 3 4 4 10 15 14 14) #f)
(testv "tests.scm" (<= 1 10 5) #f)
(testv "tests.scm" (<= 1 10 50) #t)

(testv "tests.scm" (>= 1 2 3 3 4 4 10 12 14 14) #f)
(testv "tests.scm" (>= 14 14 12 10 4 4 3 3 2 1) #t)
(testv "tests.scm" (>= 14 14 15 10 4 4 3 3 2 1) #f)
(testv "tests.scm" (>= 5 10 1) #f)
(testv "tests.scm" (>= 50 10 1) #t)

(testv "tests.scm" (nan? 1) #f)
(testv "tests.scm" (nan? 1.1) #f)
(testv "tests.scm" (nan? 1/2) #f)

;; here we show that (or) only evaluates as many arguments as is needed;
;; therefore we stick in a division by zero, which should not be evaluated,
;; and thus not give any error.
(test "tests.scm" (or (memq 'b '(a b c)) (/ 3 0)) '(b c))
(testq "tests.scm" (or) #f)
(testq "tests.scm" (or #t (/ 1 0)) #t)
(testq "tests.scm" (or 10 (/ 1 0)) 10)
(test "tests.scm" (or (quote (a b c)) (/ 1 0)) '(a b c))

;; (lambda)
(test "tests.scm" ((lambda x x) 3 4 5 6) '(3 4 5 6))
(test "tests.scm" ((lambda (x y . z) z) 3 4 5 6) '(5 6))

;; (reverse ...)
(test "tests.scm" (reverse (list 1 2 3 4)) (list 4 3 2 1))
(test "tests.scm" (reverse (list 1 2 3)) (list 3 2 1))
(test "tests.scm" (reverse (list 1 2)) (list 2 1))
(test "tests.scm" (reverse (list 1)) (list 1))

;; (abs <int>)
(testq "tests.scm" (abs -2) 2)
(testq "tests.scm" (abs -1) 1)
(testq "tests.scm" (abs 0) 0)
(testq "tests.scm" (abs 1) 1)
(testq "tests.scm" (abs 2) 2)

;; (abs <float>)
(testq "tests.scm" (abs -2.1) 2.1)
(testq "tests.scm" (abs -1.1) 1.1)
(testq "tests.scm" (abs 0.1) 0.1)
(testq "tests.scm" (abs -0.1) 0.1)
(testq "tests.scm" (abs 1.1) 1.1)
(testq "tests.scm" (abs 2.1) 2.1)

;; car and cdr
(testq "tests.scm" (caddr '(1 2 3 4)) 3)
(test "tests.scm" (cdddr '(1 2 3 4 5)) '(4 5))
(testq "tests.scm" (cadddr '(1 2 3 4 5)) 4)

;; set! and friends
(testq "tests.scm"
  (begin
    (set-cdr! (list 1 2) 3)
    123) 123) ;; just check that set-cdr! will accept
               ;; a non-symbol
(test "tests.scm"
  (let ((v (list 1 2)))
    (set-cdr! v 3)
    v)
  (cons 1 3))

(test "tests.scm"
  (let ((v (list 1 2 3)))
    (set-cdr! (cdr v) 99)
    v)
  (cons 1 (cons 2 99))) ; (1 2 . 99)

(testq "tests.scm"
  (begin
    (set-car! (list 1 2) 3)
    123) 123) ;; just check that set-car! will accept
               ;; a non-symbol
(test "tests.scm"
  (let ((v (list 1 2)))
    (set-car! v 3)
    v)
  '(3 2))

;; assq
(test "tests.scm"
  (assq
     (quote three)
     (list (list (quote one) 1)
           (list (quote two) 2)
           (list (quote three) 3)))
   (list (quote three) 3))

(test "tests.scm"
  (assq (quote two)
        (list
          (list (quote one) 1)
          (list (quote two) 2)
          (list (quote three) 3)))
  (list (quote two) 2))

(testq "tests.scm"
  (assq
    (quote threee)
    (list
      (list (quote one) 1)
      (list (quote two) 2)
      (list (quote three) 3))) #f)

;; even, odd
(testq "tests.scm" (even? 0) #t) ;; correct in MIT Scheme
(testq "tests.scm" (even? 1) #f)
(testq "tests.scm" (even? 2) #t)
(testq "tests.scm" (even? 3) #f)
(testq "tests.scm" (even? 4) #t)
(testq "tests.scm" (even? 9) #f)
(testq "tests.scm" (even? 100) #t)
(testq "tests.scm" (even? 1000) #t)
(testq "tests.scm" (odd? 0) #f) ;; correct in MIT Scheme
(testq "tests.scm" (odd? 1) #t)
(testq "tests.scm" (odd? 2) #f)
(testq "tests.scm" (odd? 3) #t)
(testq "tests.scm" (odd? 4) #f)
(testq "tests.scm" (odd? 9) #t)
(testq "tests.scm" (odd? 100) #f)
(testq "tests.scm" (odd? 1000) #f)

;; positive, negative
(testq "tests.scm" (negative? 0) #f)
(testq "tests.scm" (negative? 1) #f)
(testq "tests.scm" (negative? 2) #f)
(testq "tests.scm" (negative? 3) #f)
(testq "tests.scm" (negative? -1) #t)
(testq "tests.scm" (negative? -2) #t)
(testq "tests.scm" (negative? -3) #t)
(testq "tests.scm" (positive? 0) #f)
(testq "tests.scm" (positive? 1) #t)
(testq "tests.scm" (positive? 2) #t)
(testq "tests.scm" (positive? 3) #t)
(testq "tests.scm" (positive? -1) #f)
(testq "tests.scm" (positive? -2) #f)
(testq "tests.scm" (positive? -3) #f)

;; eqv? tests from R7RS, section 6.1
(testq "tests.scm" (eqv? (quote a) (quote a)) #t) ;; (eqv? 'a 'a)
(testq "tests.scm" (eqv? (quote a) (quote b)) #f)
(testq "tests.scm" (eqv? 2 2) #t)
(testq "tests.scm" (eqv? 2 1) #f)
(testq "tests.scm" (eqv? (list) (list)) #t) ;; (eqv? '() '()
(testq "tests.scm" (eqv? 100000000 100000000) #t)
(testq "tests.scm" (eqv? (cons 1 2) (cons 1 2)) #f)
(testq "tests.scm" (eqv? (lambda () 1) (lambda () 2)) #f)
(testq "tests.scm" (eqv? #f (quote nil)) #f)
(testq "tests.scm" (let ((p (lambda (x) x)))
            (eqv? p p)) #t)

;; unspecified tests for eqv
; (eqv? "" "")
; (eqv? '#() '#())
; (eqv? (lambda (x) x) (lambda (x) x))
; (eqv? (lambda (x) x) (lambda (y) y))

;; round
(testq "tests.scm" (round 1) 1)
(testq "tests.scm" (round 2) 2)
(testq "tests.scm" (round 0.9) 1.0)
(testq "tests.scm" (round 1.0) 1.0)
(testq "tests.scm" (round 1.1) 1.0)
(testq "tests.scm" (round 1.2) 1.0)
(testq "tests.scm" (round 1.3) 1.0)
(testq "tests.scm" (round 1.4) 1.0)
(testq "tests.scm" (round 1.5) 2.0)
(testq "tests.scm" (round 1.6) 2.0)
(testq "tests.scm" (round 2.49) 2.0)
(testq "tests.scm" (round 2.50) 3.0) ;; NOTE: Chicken and MIT Scheme reports 2! IEEE-754 magic or?
(testq "tests.scm" (round 2.51) 3.0)

;; truncate
(testq "tests.scm" (truncate 1.1) 1.0)
(testq "tests.scm" (truncate 1.4) 1.0)
(testq "tests.scm" (truncate 1.5) 1.0)
(testq "tests.scm" (truncate 1.6) 1.0)
(testq "tests.scm" (truncate 1.9) 1.0)
(testq "tests.scm" (truncate 1) 1)
(testq "tests.scm" (truncate 2) 2)

;; min
(testq "tests.scm" (min 1 2 3) 1)
(testq "tests.scm" (min 4 2 3) 2)
(testq "tests.scm" (min 4.2 2 3) 2)
(testq "tests.scm" (min 4.2 2.3 3) 2.3)
(testq "tests.scm" (min 4.2) 4.2)

;; max
(testq "tests.scm" (max 1 2 3) 3)
(testq "tests.scm" (max 4 2 3) 4)
(testq "tests.scm" (max 4.2 2 3) 4.2)
(testq "tests.scm" (max 4.2 2.3 3) 4.2)
(testq "tests.scm" (max 4.2) 4.2)

;; expt
(testq "tests.scm" (expt 2 0) 1)
(testq "tests.scm" (expt 2 1) 2)
(testq "tests.scm" (expt 2 2) 4)
(testq "tests.scm" (expt 2 3) 8)
(testq "tests.scm" (expt 3 3) 27)

;; char-whitespace
(testq "tests.scm" (char-whitespace? #\a) #f)
(testq "tests.scm" (char-whitespace? #\b) #f)
(testq "tests.scm" (char-whitespace? #\c) #f)
; TODO: Test for #\tab and friends

;; other char tests
(testq "tests.scm" (char-upcase #\a) #\A)
(testq "tests.scm" (char-upcase #\A) #\A)
(testq "tests.scm" (char-upcase #\h) #\H)
(testq "tests.scm" (char-upcase #\z) #\Z)
(testq "tests.scm" (char-downcase #\A) #\a)
(testq "tests.scm" (char-downcase #\a) #\a)
(testq "tests.scm" (char-downcase #\Z) #\z)

(test "tests.scm" (map - '(1 2 3)) '(-1 -2 -3))
(test "tests.scm" (map + '(1 2) '(3 4) '(4 5)) '(8 11))
(test "tests.scm" (map * '(1 2) '(3 4) '(4 5)) '(12 40))
(test "tests.scm" (map (lambda (a b c) (list a b c))
           '(a1 a2 a3 a4)
           '(b1 b2 b3 b4)
           '(c1 c2 c3 c4))
      '((a1 b1 c1)
        (a2 b2 c2)
        (a3 b3 c3)
        (a4 b4 c4)))
(test "tests.scm" (map (lambda (a b) (list a b)) '(a1 a2 a3) '(b1 b2 b3))
      '((a1 b1) (a2 b2) (a3 b3)))
(test "tests.scm" (map list (list)) '())
(test "tests.scm" (map list '(0)) '((0)))
(test "tests.scm" (map list '(0 1)) '((0) (1)))
(test "tests.scm" (map list '(1 2 3)) '((1) (2) (3)))
(test "tests.scm" (map list '()) '())
(test "tests.scm" (map (lambda (a b) (list b a))
           '(1 2 3 4)
           '(a b c d))
      '((a 1) (b 2) (c 3) (d 4)))
(test "tests.scm" (map (lambda (a b) (list b a))
           '(1 2 3 4)
           '(a b d))
      '((a 1) (b 2) (d 3)))

;; The following two string-map tests are from R7RS draft 6:
;;
(testq "tests.scm" (string-map char-foldcase "AbdEgH") "abdegh")
;;
(testq "tests.scm"
  (string-map
    (lambda (c)
      (integer->char (+ 1 (char->integer c))))
        "HAL") "IBM")
;;
(testq "tests.scm"
  (string-map
    (lambda (c k)
      ((if (eqv? k #\u) char-upcase char-downcase)
        c))
    "studlycaps xxx"
    "ululululul") "StUdLyCaPs")

;; Taken from R7RS draft 6
;;
(test "tests.scm"
  (let ((v '()))
    (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
        "abcde")
        v) '(101 100 99 98 97))

;; N input strings requires N parameters to the lambda:
(test "tests.scm"
  (let ((v '()))
    (string-for-each
      (lambda (one two three)
        (set! v (cons three (cons one v))))
      "ab" "cd" "efg") v)
  '(f b e a))

;; string comparison
(testq "tests.scm" (string-ci=? "foo" "FoO") #t)
(testq "tests.scm" (string-ci=? "foo" "foo") #t)
(testq "tests.scm" (string-ci=? "FOO" "FOO") #t)
(testq "tests.scm" (string-ci=? "Foo" "FOo") #t)
(testq "tests.scm" (string-ci=? "Foo" "FOoz") #f)
(testq "tests.scm" (string-ci=? "Foo" "oOo") #f)
(testq "tests.scm" (string-ci=? "foo " "foo") #f)
;;
(testq "tests.scm" (string-ci<? "AAA" "BBB") #t)
(testq "tests.scm" (string-ci<? "aaa" "BBB") #t)
(testq "tests.scm" (string-ci<? "AAA" "bbb") #t)
(testq "tests.scm" (string-ci<? "aaa" "bbb") #t)
(testq "tests.scm" (string-ci<? "AaA" "bBb") #t)
(testq "tests.scm" (string-ci<? "AaA" "AaA") #f)
(testq "tests.scm" (string-ci<? "aaA" "AAa") #f)
;;
(testq "tests.scm" (string-ci>? "AAA" "aAa") #f)
(testq "tests.scm" (string-ci>? "AAA" "AAA") #f)
(testq "tests.scm" (string-ci>? "AAA" "BBB") #f)
(testq "tests.scm" (string-ci>? "aaa" "BBB") #f)
(testq "tests.scm" (string-ci>? "AAA" "bbb") #f)
(testq "tests.scm" (string-ci>? "aaa" "bbb") #f)
(testq "tests.scm" (string-ci>? "AaA" "bBb") #f)
(testq "tests.scm" (string-ci>? "AaA" "AaA") #f)
(testq "tests.scm" (string-ci>? "aaA" "AAa") #f)
;;
(testq "tests.scm" (string-ci>? "aAa" "AAA") #f)
(testq "tests.scm" (string-ci>? "AAA" "AAA") #f)
(testq "tests.scm" (string-ci>? "BBB" "AAA") #t)
(testq "tests.scm" (string-ci>? "BBB" "aaa") #t)
(testq "tests.scm" (string-ci>? "bbb" "AAA") #t)
(testq "tests.scm" (string-ci>? "bbb" "aaa") #t)
(testq "tests.scm" (string-ci>? "BbB" "aAa") #t)
(testq "tests.scm" (string-ci>? "BbB" "BbB") #f)
(testq "tests.scm" (string-ci>=? "BbB" "BbB") #t)
(testq "tests.scm" (string-ci>? "aaA" "AAa") #f)
;;
(testq "tests.scm" (string-ci>=? "aaa" "bbb") #f)
(testq "tests.scm" (string-ci>=? "AaA" "bBb") #f)
(testq "tests.scm" (string-ci>=? "bBb" "AaA") #t)
(testq "tests.scm" (string-ci>=? "AaA" "AaA") #t)
(testq "tests.scm" (string-ci>=? "aaA" "AAa") #t)
;;
(testq "tests.scm" (string-ci<=? "aaa" "bbb") #t)
(testq "tests.scm" (string-ci<=? "aaa" "aAa") #t)
(testq "tests.scm" (string-ci<=? "AaA" "bBb") #t)
(testq "tests.scm" (string-ci<=? "AaA" "AaA") #t)
(testq "tests.scm" (string-ci<=? "aaA" "AAa") #t)

;; modulo
(testq "tests.scm" (modulo 10 6) 4)
(testq "tests.scm" (modulo 10 5) 0)
(testq "tests.scm" (modulo 10 4) 2)
(testq "tests.scm" (modulo 10 3) 1)
(testq "tests.scm" (modulo 10 2) 0)
(testq "tests.scm" (modulo 10 1) 0)
; TODO: Test negative modulo, (modulo 10 -3)

;; integer?
(testq "tests.scm" (integer? 1) #t)
(testq "tests.scm" (integer? 1.0) #t)
(testq "tests.scm" (integer? 1.1) #f)
(testq "tests.scm" (integer? (quote b)) #f)

;; char functions
(testq "tests.scm" (char->integer #\a) 97)
(testq "tests.scm" (char->integer #\b) 98)
(testq "tests.scm" (char->integer #\A) 65)
(testq "tests.scm" (char-alphabetic? #\a) #t)
(testq "tests.scm" (char-alphabetic? #\A) #t)
(testq "tests.scm" (char-alphabetic? #\2) #f)
(testq "tests.scm" (char-alphabetic? #\8) #f)
(testq "tests.scm" (char-lower-case? #\8) #f)
(testq "tests.scm" (char-lower-case? #\Z) #f)
(testq "tests.scm" (char-lower-case? #\A) #f)
(testq "tests.scm" (char-lower-case? #\H) #f)
(testq "tests.scm" (char-lower-case? #\z) #t)
(testq "tests.scm" (char-lower-case? #\a) #t)
(testq "tests.scm" (char-lower-case? #\h) #t)
(testq "tests.scm" (char-upper-case? #\8) #f)
(testq "tests.scm" (char-upper-case? #\Z) #t)
(testq "tests.scm" (char-upper-case? #\A) #t)
(testq "tests.scm" (char-upper-case? #\H) #t)
(testq "tests.scm" (char-upper-case? #\z) #f)
(testq "tests.scm" (char-upper-case? #\a) #f)
(testq "tests.scm" (char-upper-case? #\h) #f)
(testq "tests.scm" (char-numeric? #\8) #t)
(testq "tests.scm" (char-numeric? #\Z) #f)
(testq "tests.scm" (char-numeric? #\A) #f)
(testq "tests.scm" (char-numeric? #\a) #f)
(testq "tests.scm" (char-numeric? #\0) #t)
(testq "tests.scm" (char-numeric? #\1) #t)
(testq "tests.scm" (char-numeric? #\9) #t)
(testq "tests.scm" (digit-value #\0) 0)
(testq "tests.scm" (digit-value #\1) 1)
(testq "tests.scm" (digit-value #\2) 2)
(testq "tests.scm" (digit-value #\3) 3)
(testq "tests.scm" (digit-value #\4) 4)
(testq "tests.scm" (digit-value #\5) 5)
(testq "tests.scm" (digit-value #\6) 6)
(testq "tests.scm" (digit-value #\7) 7)
(testq "tests.scm" (digit-value #\8) 8)
(testq "tests.scm" (digit-value #\9) 9)
(testq "tests.scm" (digit-value #\a) #f)
(testq "tests.scm" (digit-value #\z) #f)
(testq "tests.scm" (digit-value #\x34) 4)
(testq "tests.scm" (digit-value #\x35) 5)
(testq "tests.scm" (char<=? #\9 #\0) #f)
(testq "tests.scm" (char<=? #\3 #\5) #t)
(testq "tests.scm" (char<=? #\a #\z) #t)
(testq "tests.scm" (char<=? #\a #\a) #t)
(testq "tests.scm" (char<=? #\h #\e) #f)
(testq "tests.scm" (char<=? #\r #\w) #t)
(testq "tests.scm" (char>=? #\9 #\0) #t)
(testq "tests.scm" (char>=? #\3 #\5) #f)
(testq "tests.scm" (char>=? #\a #\z) #f)
(testq "tests.scm" (char>=? #\a #\a) #t)
(testq "tests.scm" (char>=? #\h #\e) #t)
(testq "tests.scm" (char>=? #\r #\w) #f)
(testq "tests.scm" (char>? #\9 #\0) #t)
(testq "tests.scm" (char>? #\3 #\5) #f)
(testq "tests.scm" (char>? #\a #\a) #f)
(testq "tests.scm" (char>? #\h #\e) #t)
(testq "tests.scm" (char>? #\r #\w) #f)
(testq "tests.scm" (char=? #\9 #\0) #f)
(testq "tests.scm" (char=? #\3 #\5) #f)
(testq "tests.scm" (char=? #\a #\a) #t)
(testq "tests.scm" (char=? #\4 #\4) #t)
(testq "tests.scm" (char=? #\h #\e) #f)
(testq "tests.scm" (char=? #\r #\w) #f)

;; case-insensitive character predicates
(testq "tests.scm" (char-ci=? #\A #\a) #t)
(testq "tests.scm" (char-ci=? #\A #\A) #t)
(testq "tests.scm" (char-ci=? #\a #\A) #t)
(testq "tests.scm" (char-ci=? #\a #\a) #t)
(testq "tests.scm" (char-ci=? #\a #\b) #f)
(testq "tests.scm" (char-ci=? #\a #\b) #f)
(testq "tests.scm" (char-ci=? #\A #\B) #f)
(testq "tests.scm" (char-ci=? #\A #\b) #f)

;; conversion
(testq "tests.scm" (integer->char 65) (quote #\A))
(testq "tests.scm" (integer->char 97) (quote #\a))
(testq "tests.scm" (list->string (list #\a #\b #\c)) "abc")
(testq "tests.scm" (list->string (list #\a #\b #\b #\A)) "abbA")
(test "tests.scm" (list-tail (list 1 2 3) 0) (list 1 2 3))
(test "tests.scm" (list-tail (list 1 2 3) 1) (list 2 3))
(test "tests.scm" (list-tail (list 1 2 3) 2) (list 3))
(test "tests.scm" (list-tail (list 1 2 3) 3) (list))

;; member
(testq "tests.scm" (member 10 (list 1 2 3)) #f)
(test "tests.scm" (member 10 (list 10 20 30)) (list 10 20 30))
(test "tests.scm" (member 20 (list 10 20 30)) (list 20 30))
(test "tests.scm" (member 30 (list 10 20 30)) (list 30))
(testq "tests.scm" (member 40 (list 10 20 30)) #f)
(test "tests.scm" (member 20
               (list 10 20 30
                     (quote bee)
                     (quote cee)))
       (list 20 30 (quote bee) (quote cee)))


;; memv (TODO: insert eqv? specific check)
(testq "tests.scm" (memv 10 (list 1 2 3)) #f)
(test "tests.scm" (memv 10 (list 10 20 30)) (list 10 20 30))
(test "tests.scm" (memv 20 (list 10 20 30)) (list 20 30))
(test "tests.scm" (memv 30 (list 10 20 30)) (list 30))
(testq "tests.scm" (memv 40 (list 10 20 30)) #f)
(test "tests.scm" (memv 20 (list 10 20 30
                      (quote bee)
                      (quote cee)))
       (list 20 30 (quote bee) (quote cee)))

;; memq (TODO: insert eq? specific check)
(testq "tests.scm" (memq 10 (list 1 2 3)) #f)
(test "tests.scm" (memq 10 (list 10 20 30)) (list 10 20 30))
(test "tests.scm" (memq 20 (list 10 20 30)) (list 20 30))
(test "tests.scm" (memq 30 (list 10 20 30)) (list 30))
(testq "tests.scm" (memq 40 (list 10 20 30)) #f)
(test "tests.scm" (memq 20 (list 10 20 30
                      (quote bee) (quote cee)))
       (list 20 30 (quote bee) (quote cee)))

;; gcd, lcm
(testq "tests.scm" (gcd) 0)
(testq "tests.scm" (gcd 10) 10)
(testq "tests.scm" (gcd -10) 10)
(testq "tests.scm" (gcd 10 2) 2)
(testq "tests.scm" (gcd 10 3) 1)
(testq "tests.scm" (gcd 10 5) 5)
(testq "tests.scm" (gcd 5 10) 5)
(testq "tests.scm" (gcd 32 -36) 4)
(testq "tests.scm" (gcd -32 36) 4)
(testq "tests.scm" (gcd -32 -36) 4)
(testq "tests.scm" (gcd 4 6 8 10) 2)
(testq "tests.scm" (gcd 4 6 8 12) 2)
(testq "tests.scm" (gcd 40 24) 8)
(testq "tests.scm" (gcd 1230 4560) 30)
(testq "tests.scm" (lcm) 1)
(testq "tests.scm" (lcm 10) 10)
(testq "tests.scm" (lcm -10) 10)
(testq "tests.scm" (lcm 10 10) 10)
(testq "tests.scm" (lcm 4 6) 12)
(testq "tests.scm" (lcm 6 4) 12)
(testq "tests.scm" (lcm 2 4 6) 12)
(testq "tests.scm" (lcm 2 4 6 8) 24)
(testq "tests.scm" (lcm 2 4 6 -8) 24)
(testq "tests.scm" (lcm 2 -4 6 -8) 24)
(testq "tests.scm" (lcm 201 202 203) 8242206)
(testq "tests.scm" (lcm 201 202 203 204) 280235004)

;; list-ref
(testq "tests.scm" (list-ref (list 1 2 3) 2) 3)
(testq "tests.scm" (list-ref (list 1 2 3) 1) 2)
(testq "tests.scm" (list-ref (list 1 2 3) 0) 1)
(testq "tests.scm" (list-ref (list 1 2 3 4) 0) 1)
(testq "tests.scm" (list-ref (list 1 2 3 4) 3) 4)

;; string conversion
(test "tests.scm" (string->list "hey") (list #\h #\e #\y))
(testq "tests.scm" (string->symbol "hey") (quote hey))
(testq "tests.scm" (string->number "abba") #f)
(testq "tests.scm" (string->number "123") 123)
(testq "tests.scm" (string->number "456") 456)
(testq "tests.scm" (string->number "1.2") 1.2)
(testq "tests.scm" (string->number "1.5") 1.5)
(testq "tests.scm" (string-length "abba") 4)
(testq "tests.scm" (string-length "abb") 3)
(testq "tests.scm" (string-length "ab") 2)
(testq "tests.scm" (string-length "a") 1)
(testq "tests.scm" (string-length "") 0)
(testq "tests.scm" (substring "Hello!" 0 0) "")
(testq "tests.scm" (substring "Hello!" 0 1) "H")
(testq "tests.scm" (substring "Hello!" 0 3) "Hel")
(testq "tests.scm" (substring "Hello!" 1 3) "ell")
(testq "tests.scm" (substring "Hello!" 1 3) "ell")
(testq "tests.scm" (substring "Hello!" 1 4) "ello")
(testq "tests.scm" (substring "Hello!" 2 4) "llo!")
(testq "tests.scm" (string=? "hey" "hey") #t)
(testq "tests.scm" (string=? "hey" "heya") #f)
(testq "tests.scm" (string<=? "hey" "heya") #t)
(testq "tests.scm" (string-ref "hey" 0) #\h)
(testq "tests.scm" (string-ref "hey" 1) #\e)
(testq "tests.scm" (string-ref "hey" 2) #\y)

;; append tests
(testq "tests.scm" (append (list) 1) 1)
(test "tests.scm" (append (list) (list 1 2)) (list 1 2))
(test "tests.scm" (append (list) (list 1 2)) (list 1 2))
(test "tests.scm" (append (list 1)) (list 1))
(test "tests.scm" (append (list 1)) (list 1))
(test "tests.scm" (append (list 1) 2) (cons 1 2))
(test "tests.scm" (append (list 1) (list 3)) (list 1 3))
(test "tests.scm" (append (list 1) (list 3 4)) (list 1 3 4))
(test "tests.scm" (append (list 1) (list 3 4) 5) (cons 1 (cons 3 (cons 4 5))))
(test "tests.scm" (append
         (append
           (list 1 2 3)
           (list 4))
         (list 5 6)
           7)
       (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))

;; Regression test: append should not mutate
(test "tests.scm"
  (begin
    (define a (list 1))
    (append a 2)
    (append a 3)
    a) (list 1))

;; Quoting
(test "tests.scm" 'hey (quote hey))
(test "tests.scm" 'hey 'hey)
(test "tests.scm" ''hey ''hey)
(test "tests.scm" (append '(1 2 3) 4) (cons 1 (cons 2 (cons 3 4))))
(testq "tests.scm" (apply + '(1 2 3)) 6)
(testq "tests.scm" (apply + '(1 2 3)) 6) ; is double-quote ok?
;
(test "tests.scm" (quasiquote (1 2 (unquote (+ 3 4)))) '(1 2 7)) ; this test "tests.scm" should work nicely (TODO)
(test "tests.scm" (quasiquote (1 2 (unquote (+ 3 4)))) (list 1 2 7)) ; this test "tests.scm" should work nicely (TODO)
(test "tests.scm" `(1 2 ,(+ 3 4) 3 y) '(1 2 7 3 y))
;
; Quasiquoting with unquote-splicing ",@"
;(testq "tests.scm" (quote (define foo '(11 22 33)) `(1 2 3 foo ,foo ,@foo)) (quote (1 2 3 foo (11 22 33) 11 22 33)))
(test "tests.scm" `(1 2 ,@(list 3 4 5) 6 7) (list 1 2 3 4 5 6 7))
(test "tests.scm" (let ((a '(11 22 33))) `(+ ,@a 44)) '(+ 11 22 33 44))

;; Math module
(testq "tests.scm" (ceiling 3.0) 3.0)
(testq "tests.scm" (ceiling 3.1) 4.0)
(testq "tests.scm" (ceiling 3.4) 4.0)
(testq "tests.scm" (ceiling 3.5) 4.0)
(testq "tests.scm" (ceiling 3.6) 4.0)
(testq "tests.scm" (floor 3.0) 3.0)
(testq "tests.scm" (floor 3.1) 3.0)
(testq "tests.scm" (floor 3.4) 3.0)
(testq "tests.scm" (floor 3.5) 3.0)
(testq "tests.scm" (floor 3.6) 3.0)
(testq "tests.scm" (floor 3.9) 3.0)
(testq "tests.scm" (floor 3.999) 3.0)
(test "tests.scm" (sqrt 3.999) 1.99975)
(testq "tests.scm" (sqrt 4.0) 2.0)
(test "tests.scm" (sqrt -4.0) (sqrt -1)) ; nan
(test "tests.scm" (exp 1) 2.71828)
(test "tests.scm" (exp 2) 7.38906)
(test "tests.scm" (atan 45.0) 1.54858)
(test "tests.scm" (atan 12.3) 1.48967)
(test "tests.scm" (acos 1.0) 0.0)
(test "tests.scm" (acos 0.5) 1.0472)
(test "tests.scm" (asin 1.0) 1.5708)
(test "tests.scm" (asin 0.5) 0.523599)
(test "tests.scm" (tan 0.5) 0.546302)
(test "tests.scm" (cos 0.5) 0.877583)
(test "tests.scm" (sin (* 0.5 3.1415926535)) 1.0)
(test "tests.scm" (log 256) 5.54518)
(test "tests.scm" (/ (log 256) (log 2)) 8.0)
(test "tests.scm" (/ (log 1024) (log 2)) 10.0)

;; Number system
(testq "tests.scm" (real? 3) #t) ; from r7rs draft

;; Characters
(testq "tests.scm" (string-length (make-string 0)) 0)
(testq "tests.scm" (string-length (make-string 1)) 1)
(testq "tests.scm" (string-length (make-string 2)) 2)
(testq "tests.scm" (string-length (make-string 42)) 42)
(testq "tests.scm" (string-length (make-string 42 #\a)) 42)
(testq "tests.scm" (string-length (make-string 42 #\0)) 42)
(test "tests.scm" (make-string 1 #\x28) "(")
(test "tests.scm" (make-string 2 #\x28) "((")
(test "tests.scm" (make-string 3 #\x28) "(((")
(test "tests.scm" (make-string 3 #\x29) ")))")
(test "tests.scm" (make-string 5 #\a) "aaaaa")
(test "tests.scm" (make-string 5 #\A) "AAAAA")
(test "tests.scm" (make-string 7 #\space) "       ")

;; TODO/BUG: Here's another one.  When parsing "#\newline", it is converted
;; to char and then displayed by sprint() / print() as a newline, instead of
;; the literal "#\newline". Fix that.
(testq "tests.scm" (string-length (make-string 9 #\newline)) 9)
(testq "tests.scm" (string-length (make-string 7 #\tab)) 7)
(testq "tests.scm" (string-length (make-string 3 #\x1)) 3)

;; The following two tests are BUGs in our implementation, because we use zero
;; terminated strings internally for the STRING type, and therefore
;; cannot count using strlen().  Either switch to std::string or reimplement
;; STRING.
(xfailq "tests.scm" (string-length (make-string 3 #\x0)) 3 "TODO: Add embedded null chars.")
(xfailq "tests.scm" (string-length (make-string 3 #\null)) 3 "TODO: Add embedded null chars.")

;; Special character literals
(testq "tests.scm" (length (list #\( #\))) 2)
(testq "tests.scm" (vector-length #(1 2 3 #\))) 4)

;; Comment-out datum
(testq "tests.scm" (+ 1 #; 2 3) 4)
(testq "tests.scm" (+ 1 #;2 3) 4)

;; Quasiquote
(test "tests.scm" `(1 2 ,(+ 3 4)) '(1 2 7))
(testq "tests.scm" '() (list))
(testq "tests.scm" '() '())
(testq "tests.scm" (list) '())
(testq "tests.scm" (list) '())

;; Test "#;", or "Ignore next form":
(test "tests.scm" `(a #;(this should be ignored) b c) '(a b c))
(test "tests.scm" `(a #; (this should also be ignored) b c) '(a b c))
(test "tests.scm" `(a #; zz b c) '(a b c))

;; Vectors
(testq "tests.scm" (vector-length (vector 1 2 3)) 3)
(testq "tests.scm" (vector-length (vector)) 0)
(testq "tests.scm" (vector-length (vector 1 2 foo)) 3) ; self-evaluating elements
(testq "tests.scm"
  (vector-length
    (vector 1 2 (define (v foo) (* foo foo)) 100)) 4)

(testq "tests.scm" (list? (cons 1 2)) #f)
(testq "tests.scm" (list? (quote (a . b))) #f)
(testq "tests.scm" (pair? (cons 1 2)) #t)
(testq "tests.scm" (pair? (quote (a . b))) #t)
(testq "tests.scm" (length (list '() '())) 2)

; to fix below bugs, tokenizer should must return 3 tokens for "a""b""c"
(testq "tests.scm" (length '(#;"a""b""c")) 2)
(testq "tests.scm" (length '("a"#;"b""c""d""e")) 4)

; to fix, enable support for \x<hex-number> escapes in string parser
(testq "tests.scm" (string-length "a\x42;c") 3)
(testq "tests.scm" "a\x42;c" "aBc")

(test "tests.scm" (map (lambda (x) (* x x)) '(0 1 2 3 4 5)) '(0 1 4 9 16 25))

;; Exactness
(testq "tests.scm" (exact? 3.4) #f)
(testq "tests.scm" (exact? 10.0) #f)
(testq "tests.scm" (exact? (* 2 2)) #t)
(testq "tests.scm" (exact? (* 2 2.0)) #f)
(testq "tests.scm" (exact? (+ 1 2 3)) #t)
(testq "tests.scm" (exact? (+ 1 2 3 (* 10 20))) #t)
(testq "tests.scm" (exact? (+ 1 2 3 (* 10 20 4))) #t)
(testq "tests.scm" (exact? (+ 1 2 3 (* 10 20 4 1))) #t)
(testq "tests.scm" (exact? (+ 1 2 3 (* 10 20 4 1.0))) #f)
(testq "tests.scm" (exact? (exact 1)) #t)
(testq "tests.scm" (exact? (exact 1.1)) #t)
(testq "tests.scm" (exact? (exact -1.1)) #t)
(testq "tests.scm" (exact? (exact -1)) #t)
(testq "tests.scm" (exact? (exact 0)) #t)
(testq "tests.scm" (exact? #e123) #t)
(testq "tests.scm" (inexact? #e123) #f)
(testq "tests.scm" (exact? #i123) #f)
(testq "tests.scm" (inexact? #i123) #t)
(testq "tests.scm" (exact? #e123.456) #t)
(testq "tests.scm" (inexact? #e123.456) #f)
(testq "tests.scm" (inexact? #i123.456) #t)
(testq "tests.scm" (exact? #i123.456) #f)
(test "tests.scm" (exact 10) 10)
(test "tests.scm" (exact 10.2) 51/5)
(test "tests.scm" (exact 0.1) 1/10)
(test "tests.scm" (exact 0.2) 2/10)
(test "tests.scm" (exact 0.02) 2/100)
(test "tests.scm" (exact 0.002) 2/1000)
(test "tests.scm" (exact 1.1) 11/10)
(test "tests.scm" (exact 1.2) 6/5)
(test "tests.scm" (equal? (inexact 10/2) (inexact 5)) #t)
(test "tests.scm" (inexact 10/2) (inexact 5))
(test "tests.scm" (inexact 10/2) 5.0)
(test "tests.scm" (inexact 1/2) 0.5)
(test "tests.scm" (inexact 1/2) .5)
(test "tests.scm" (inexact? (inexact 5)) #t)
(test "tests.scm" (inexact 10/3) 3.33333)
(test "tests.scm" (exact 123.456) 15432/125)
(test "tests.scm" (exact .1) 1/10)
(test "tests.scm" (exact .01) 1/100)
(test "tests.scm" (exact .001) 1/1000)
(test "tests.scm" (exact .0001) 1/10000)
(test "tests.scm" (exact .00001) 1/100000)
(test "tests.scm" (exact .000001) 1/1000000)
(test "tests.scm" (exact .0000001) 1/10000000)
(test "tests.scm" (exact .0000002) 1/5000000)
(test "tests.scm" (exact .0000003) 3/10000000)

;; Empty list and open-output-string
;; (note, we don't support open-output-string yet)
;(test "tests.scm" (let ((s (open-output-string)))
;        (display '(lambda () 1 2) s)
;        (get-output-string s)) "(lambda () 1 2)")
(test "tests.scm" (cons 1 '()) '(1))
(test "tests.scm" (cons 1 '()) (list 1))
(test "tests.scm" (cons 2 (cons 1 (list))) '(2 1))
(test "tests.scm" (cons 2 (cons 1 (list))) (list 2 1))
(test "tests.scm" (cons 2 (cons 1 '())) '(2 1))
(test "tests.scm" (cons 2 (cons 1 '())) (list 2 1))
(test "tests.scm" (length (cons 1 '())) 1)
(test "tests.scm" (length (cons 2 (cons 1 '()))) 2)
(test "tests.scm" (length (list '())) 1)
(test "tests.scm" (length (list '() '())) 2)
(test "tests.scm" (pair? '()) #f)
(test "tests.scm" (pair? '(())) #t)
(test "tests.scm" (pair? (list (list))) #t)
(test "tests.scm" (pair? (list)) #f)
(test "tests.scm" (null? '()) #t)
(test "tests.scm" (null? '(())) #f)

;; Subtraction errors
(test "tests.scm" (exact? (- 1 1)) #t)
(test "tests.scm" (integer? (- 1 1)) #t)
(test "tests.scm" (integer? (- 10 10)) #t)
(test "tests.scm" (integer? (- 1 1)) #t)
(test "tests.scm" (integer? (- 3 1 1 1)) #t)
(test "tests.scm" (- 1 1) #e0)
(test "tests.scm" (- 3 1 1 1) 0)
(test "tests.scm" (integer? (- 3 1 1 1)) #t)

;; Tokenizer should recognize "a""b" as two tokens "a" and "b"
(test "tests.scm" '("1""2""3") (list "1" "2" "3"))
(test "tests.scm" '("foo""bar""baz") (list "foo" "bar" "baz"))
(test "tests.scm" (length '("foo""bar""baz")) 3)
;; Escaped characters should still work
(test "tests.scm" (string-length "Say: \"Hello!\".") 14)
(test "tests.scm" (length (list "Backslash: \\""Another string")) 2)
(test "tests.scm" (length (list "Backslash: \\""Another string")) 2)
(test "tests.scm" (string-ref (car (list "Backslash: \\""Another string")) 11) #\x5c)

;; Symbols enclosed in pipes, format "|a b c|"
(test "tests.scm" (let ((|a b| 123)) |a b|) 123)
(test "tests.scm" (let ((|a  b | 123) (|c d e| 876)) (+ |a  b | |c d e|)) 999)
(test "tests.scm" (symbol->string '|foo bar  baz |) "|foo bar  baz |")
(test "tests.scm" (let ((|squared number| (lambda (x) (* x x))))
        (|squared number| 12)) 144)

;; Call-with-values
(test "tests.scm" (call-with-values
        (lambda () (values -11 1880))
        (lambda (a b) (+ (* a a) b))) 2001)

;; define-macro, aka defmacro, tests
(let ()
  (define-macro defmacro-foo1 (a b)
    ;; basic pattern, ",a" means "EVALUATE argument a",
    ;; while "a" means "just use a as-is"
    `(+ ,a (* ,b 10)))
  (testq "tests.scm" (defmacro-foo1 3 2) 23)
  (testq "tests.scm" (defmacro-foo1 (+ 1 2) 2) 23))

;; Note on above: if we did `(+ a (,b * 10))
;; without using ",a" then it would try to evaluate
;; (+ <a unevaluated> (* <value of b> 10)).
;; TODO: Add a test "tests.scm" for this.

;; Other tests
(test "tests.scm" (make-list 3 2) '(2 2 2))
(test "tests.scm" (make-list 3) '(#f #f #f))
(test "tests.scm" (make-list 2 '(a b)) '((a b) (a b)))
