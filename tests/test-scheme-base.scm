(import (scheme base)
        (test unit-test)
        (only (scheme write) display))

(define name "(scheme base)")

;; Arithmetic
(testq name (+ 1 2 3 4) 10)
(testv name (+ 1 2 3 4) 10)
(test name (+ 1 2 3 4) 10)
(testq name (* 1 2 3 4) 24)
(testq name (- 1 2 3 4) -8)
(testq name (- 0.5 -1.5) 2.0)
(testq name (- 0.5 -10.5) 11.0)
(testq name (- 1 2 3) -4)
(testq name (- 1) -1)
(testq name (- 2) -2)
(testq name (- -2) 2)


;; Strings
(testq name (string-append "a" "b" "b" "a") "abba")
(testq name (number->string 12345) "12345")

;; Lists
(test name (list 1 2 (list 3 4)) (list 1 2 (list 3 4)))

;; Apply
(testq name (apply + (list 1 2 3)) 6)
(testq name (apply + (quote (1 2 3))) 6)
(testq name (apply + (list 1 2 3 (* 5 6))) 36)
(testq name (apply + (list 1 2 3 (* 5 5))) 31)

;; Exactness
(testq name (exact? (- 0.5 -1.5)) #f) ; is this really universally true?

;; Predicates
(testq name (number? 'a) #f)
(testq name (number? +) #f)
(testq name (number? 1) #t)
(testq name (number? 2) #t)
(testq name (number? 1.0) #t)
(testq name (number? 0) #t)
(testq name (number? 0.0) #t)
(testq name (number? 0.000) #t)
(testq name (number? -0.000) #t)
(testq name (number? -1.000) #t)
(testq name (number? -1.234) #t)
(testq name (number? 1.234) #t)
(testq name (number? 1234) #t)

;; Rational numbers
(testq name (rational? 1) #t)
(testq name (rational? 1.0) #t)
(testq name (rational? 2.0) #t)
(testq name (rational? 2.1) #t)
(testq name (rational? -10) #t)
(testq name (rational? 0) #t)
(testq name (rational? 10/20) #t)
(testq name (= 10/20 1/2) #t)
(testq name (= 0.5 1/2) #t)
(testq name (= 0.501 1/2) #f)
(testq name (= 0.6 1/2) #f)
(testq name (= 0.51 1/2) #f)
(testq name 1000/200 5)

;; Rational arithmetic
(test name (* 1/2 1/2) 1/4)
(test name (exact? (* 1/2 1/2)) #t)
(test name (* 1/2 1/2 2) 1/2)
(test name (* 1/2 1/2 2 2) 1)
(test name (exact? (* 1/2 1/2 2 2)) #t)
(test name (exact? (* 1/2 1/2 2 #i2)) #f)
(test name (* 1/2 1/2 2 3) 3/2)
(test name (rational? (* 1/2 1/2 2 3)) #t)
(test name (* 1/2 1/2 2 10) 5)
(test name (integer? (* 1/2 1/2 2 10)) #t)
(test name (exact? (* 1/2 1/2 2 10)) #t)
(test name (inexact? (* 1/2 1/2 2 10)) #f)
(test name (inexact? (* 1/2 1/2 #i2 10)) #t)
(test name (+ 1/2 1/2) 1)
(test name (+ 1/4 1/4) 1/2)
(test name (+ 1/2 0.2) 0.7)
(test name (+ 1/2 1) 3/2)
(testq name (exact? (+ 1/2 1/2)) #t)
(testq name (exact? (+ 1/4 1/4)) #t)
(testq name (exact? (+ 1/2 0.2)) #f)
(testq name (exact? (+ 1/2 1)) #t)

;; Zero predicate
(test name (zero? -1/2) #f)
(test name (zero? -0.2) #f)
(test name (zero? -1) #f)
(test name (zero? 0) #t)
(test name (zero? 0/1) #t)
(test name (zero? 0.0) #t)
(test name (zero? 1) #f)
(test name (zero? 1/2) #f)
(test name (zero? 0.2) #f)

;; Positive and negative infinities

(test name (< -inf.0 0) #t)
(test name (> -inf.0 0) #f)

(test name (> +inf.0 0) #t)
(test name (< +inf.0 0) #f)

(test name (- -inf.0) +inf.0)
(test name (- +inf.0) -inf.0)

(test name (infinite? -inf.0) #t)
(test name (infinite? +inf.0) #t)
(test name (infinite? 123) #f)
(test name (infinite? -123) #f)

; negative infinity
(test name (number->string (/ 10 -inf.0)) "-0")

(test name (+ 1 +nan.0) +nan.0)

;; Number radix / prefix
(testq name #xFF 255)
(testq name #xFFF 4095)
(testq name #b11 3)
(testq name #b01001001100101100000001011010010 1234567890)
(testq name #o77 63)
(testq name #d123 123)
;; ... with exactness
(test name #e1.1 11/10)
(test name #e1.2 6/5)

;; Test exactness
(testq name (exact? 1) #t)
(testq name (exact? 1.1) #f)
(testq name (exact? (+ 1 2 3)) #t)
(testq name (exact? (+ 1 2/3 3)) #t)
(testq name (exact? (* 1 2/3 3)) #t)
(testq name (exact? (* 1 2/3 #i3)) #f)
(testq name (exact? (* #i1 2/3 3)) #f)
(testq name (exact? (/ 8 2)) #t)
(testq name (exact? (/ 10 10)) #t)


;; pair?, list? and dot notation
(test name '(1 . 2) (cons 1 2))
(testq name (pair? '(1 . 3)) #t)
(testq name (pair? (cons 1 3)) #t)
(testq name (list? '(1 . 3)) #f)
(testq name (list? (cons 1 3)) #f)
(testq name (pair? (list 1 2)) #t)
(testq name (list? (list 1 2)) #t)
(testq name (pair? '()) #f)
(testq name (list? '()) #t)
(testq name (list? 123) #f)
(testq name (pair? 123) #f)
(testq name (+ 1 . (2 . (3 . ()))) 6)
(testq name (list? '(1 . 2)) #f)

;; (if t a1 a2)
(testq name (if (> 4 2) 11 22) 11)
(testq name (if (> 3 2) 11 22) 11)
(testq name (if (> 2 2) 11 22) 22)
(testq name (if (> 1 2) 11 22) 22)

;; (if t a1)
(testq name (if (> 3 2) 11) 11)
(testq name (if (< 1 2) 11) 11)

;; (and)
(testq name (and (= 2 2) (> 2 1)) #t)
(testq name (and (= 2 2) (< 2 1)) #f)
(test name (and 1 2 'c '(f g)) '(f g))
(testq name (and) #t)
(testq name (and 1) 1)
(testq name (and 0) 0)
(testq name (and 0 #f) #f)
(testq name (and 1 #f 2) #f)
(test name (and 1 2 (quote c) (quote (f g))) '(f g))

;; (or)
(testq name (or (= 2 2) (> 2 1)) #t)
(testq name (or (= 2 2) (< 2 1)) #t)
(testq name (or #f #f #f) #f)
(testq name (or #t #f #f) #t)
(testq name (or #f #t #f) #t)
(testq name (or #f #f #t) #t)
(test name (or 1 (/ 0)) 1)

;; Equal operator
(testv name (= 1 1 1 1 1) #t)
(testv name (= 1 1 1 1 1.0 1 1.0 1.0 1) #t)
(testv name (= 1 1 2 1 1.0 1 1.0 1.0 1) #f)
(testv name (= 1 1 2 1 1.0 1 1.0 1.01 1) #f)
(testv name (= 1 1 1 1 1.0 1 1.0 1.01 1) #f)
(testv name (= 0.51 0.51 0.51 0.51) #t)
(testv name (= 0.51 0.51 0.52 0.51) #f)
(testv name (= 1/2 1/2 1/2 0.5 10/20 11/22 (* 0.1 5)) #t)

;; Comparison operators
(testv name (< 1 2) #t)
(testv name (< 2 1) #f)
(testv name (< 1 2 3) #t)
(testv name (< -1 2 3) #t)
(testv name (< -2 -1 0 1 2 3) #t)
(testv name (< -2 -1 0 1 2 3 2) #f)
(testv name (< -2 -1 0 1 2 3 4) #t)
(testv name (< -2 -1 -1 1 2 3 4) #f)
(testv name (< -2 -1 -2 1 2 3 4) #f)
(testv name (< -2 -1 1 2 3 4) #t)
(testv name (< -2 -1 +nan.0 2 3 4) #f)

(testv name (> 2 1) #t)
(testv name (> 3 2 1) #t)
(testv name (> 1 2) #f)
(testv name (> 3 2) #t)
(testv name (> 2 3) #f)
(testv name (> 4 3 2 1) #t)
(testv name (> 4 2 0 -1) #t)
(testv name (> -1 -2) #t)
(testv name (> -10 -20) #t)
(testv name (> -10 -20 -30) #t)
(testv name (> -10 +nan.0 -30) #f)
(testv name (> -10 -20 -4) #f)

(testv name (<= 1 2 3 3 4 4 10 12 14 14) #t)
(testv name (<= 1 2 3 3 4 4 10 15 14 14) #f)
(testv name (<= 1 10 5) #f)
(testv name (<= 1 10 50) #t)

(testv name (>= 1 2 3 3 4 4 10 12 14 14) #f)
(testv name (>= 14 14 12 10 4 4 3 3 2 1) #t)
(testv name (>= 14 14 15 10 4 4 3 3 2 1) #f)
(testv name (>= 5 10 1) #f)
(testv name (>= 50 10 1) #t)

(testv name (nan? 1) #f)
(testv name (nan? 1.1) #f)
(testv name (nan? 1/2) #f)

;; Logical operators

;; Here we show that (or) only evaluates as many arguments as is needed;
;; therefore we stick in a division by zero, which should not be evaluated, and
;; thus not give any error.
(test name (or (memq 'b '(a b c)) (/ 3 0)) '(b c))
(testq name (or) #f)
(testq name (or #t (/ 1 0)) #t)
(testq name (or 10 (/ 1 0)) 10)
(test name (or (quote (a b c)) (/ 1 0)) '(a b c))

;; (lambda)
(test name ((lambda x x) 3 4 5 6) '(3 4 5 6))
(test name ((lambda (x y . z) z) 3 4 5 6) '(5 6))

;; (reverse ...)
(test name (reverse (list 1 2 3 4)) (list 4 3 2 1))
(test name (reverse (list 1 2 3)) (list 3 2 1))
(test name (reverse (list 1 2)) (list 2 1))
(test name (reverse (list 1)) (list 1))

;; (abs <int>)
(testq name (abs -2) 2)
(testq name (abs -1) 1)
(testq name (abs 0) 0)
(testq name (abs 1) 1)
(testq name (abs 2) 2)

;; (abs <float>)
(testq name (abs -2.1) 2.1)
(testq name (abs -1.1) 1.1)
(testq name (abs 0.1) 0.1)
(testq name (abs -0.1) 0.1)
(testq name (abs 1.1) 1.1)
(testq name (abs 2.1) 2.1)

;; set! and friends
(testq name
  (begin
    (set-cdr! (list 1 2) 3)
    123) 123) ;; just check that set-cdr! will accept
               ;; a non-symbol
(test name
  (let ((v (list 1 2)))
    (set-cdr! v 3)
    v)
  (cons 1 3))

(test name
  (let ((v (list 1 2 3)))
    (set-cdr! (cdr v) 99)
    v)
  (cons 1 (cons 2 99))) ; (1 2 . 99)

(testq name
  (begin
    (set-car! (list 1 2) 3)
    123) 123) ;; just check that set-car! will accept
               ;; a non-symbol
(test name
  (let ((v (list 1 2)))
    (set-car! v 3)
    v)
  '(3 2))

;; assq
(test name
  (assq
     (quote three)
     (list (list (quote one) 1)
           (list (quote two) 2)
           (list (quote three) 3)))
   (list (quote three) 3))

(test name
  (assq (quote two)
        (list
          (list (quote one) 1)
          (list (quote two) 2)
          (list (quote three) 3)))
  (list (quote two) 2))

(testq name
  (assq
    (quote threee)
    (list
      (list (quote one) 1)
      (list (quote two) 2)
      (list (quote three) 3))) #f)

;; even, odd
(testq name (even? 0) #t) ;; correct in MIT Scheme
(testq name (even? 1) #f)
(testq name (even? 2) #t)
(testq name (even? 3) #f)
(testq name (even? 4) #t)
(testq name (even? 9) #f)
(testq name (even? 100) #t)
(testq name (even? 1000) #t)
(testq name (odd? 0) #f) ;; correct in MIT Scheme
(testq name (odd? 1) #t)
(testq name (odd? 2) #f)
(testq name (odd? 3) #t)
(testq name (odd? 4) #f)
(testq name (odd? 9) #t)
(testq name (odd? 100) #f)
(testq name (odd? 1000) #f)

;; positive, negative
(testq name (negative? 0) #f)
(testq name (negative? 1) #f)
(testq name (negative? 2) #f)
(testq name (negative? 3) #f)
(testq name (negative? -1) #t)
(testq name (negative? -2) #t)
(testq name (negative? -3) #t)
(testq name (positive? 0) #f)
(testq name (positive? 1) #t)
(testq name (positive? 2) #t)
(testq name (positive? 3) #t)
(testq name (positive? -1) #f)
(testq name (positive? -2) #f)
(testq name (positive? -3) #f)

;; eqv? tests from R7RS, section 6.1
(testq name (eqv? (quote a) (quote a)) #t) ;; (eqv? 'a 'a)
(testq name (eqv? (quote a) (quote b)) #f)
(testq name (eqv? 2 2) #t)
(testq name (eqv? 2 1) #f)
(testq name (eqv? (list) (list)) #t) ;; (eqv? '() '()
(testq name (eqv? 100000000 100000000) #t)
(testq name (eqv? (cons 1 2) (cons 1 2)) #f)
(testq name (eqv? (lambda () 1) (lambda () 2)) #f)
(testq name (eqv? #f (quote nil)) #f)
(testq name (let ((p (lambda (x) x)))
            (eqv? p p)) #t)

;; unspecified tests for eqv
; (eqv? "" "")
; (eqv? '#() '#())
; (eqv? (lambda (x) x) (lambda (x) x))
; (eqv? (lambda (x) x) (lambda (y) y))

;; round
(testq name (round 1) 1)
(testq name (round 2) 2)
(testq name (round 0.9) 1.0)
(testq name (round 1.0) 1.0)
(testq name (round 1.1) 1.0)
(testq name (round 1.2) 1.0)
(testq name (round 1.3) 1.0)
(testq name (round 1.4) 1.0)
(testq name (round 1.5) 2.0)
(testq name (round 1.6) 2.0)
(testq name (round 2.49) 2.0)
(testq name (round 2.50) 3.0) ;; NOTE: Chicken and MIT Scheme reports 2! IEEE-754 magic or?
(testq name (round 2.51) 3.0)

;; truncate
(testq name (truncate 1.1) 1.0)
(testq name (truncate 1.4) 1.0)
(testq name (truncate 1.5) 1.0)
(testq name (truncate 1.6) 1.0)
(testq name (truncate 1.9) 1.0)
(testq name (truncate 1) 1)
(testq name (truncate 2) 2)

;; min
(testq name (min 1 2 3) 1)
(testq name (min 4 2 3) 2)
(testq name (min 4.2 2 3) 2)
(testq name (min 4.2 2.3 3) 2.3)
(testq name (min 4.2) 4.2)

;; max
(testq name (max 1 2 3) 3)
(testq name (max 4 2 3) 4)
(testq name (max 4.2 2 3) 4.2)
(testq name (max 4.2 2.3 3) 4.2)
(testq name (max 4.2) 4.2)

;; expt
(testq name (expt 2 0) 1)
(testq name (expt 2 1) 2)
(testq name (expt 2 2) 4)
(testq name (expt 2 3) 8)
(testq name (expt 3 3) 27)

;; Map
(test name (map - '(1 2 3)) '(-1 -2 -3))
(test name (map + '(1 2) '(3 4) '(4 5)) '(8 11))
(test name (map * '(1 2) '(3 4) '(4 5)) '(12 40))
(test name (map (lambda (a b c) (list a b c))
           '(a1 a2 a3 a4)
           '(b1 b2 b3 b4)
           '(c1 c2 c3 c4))
      '((a1 b1 c1)
        (a2 b2 c2)
        (a3 b3 c3)
        (a4 b4 c4)))
(test name (map (lambda (a b) (list a b)) '(a1 a2 a3) '(b1 b2 b3))
      '((a1 b1) (a2 b2) (a3 b3)))
(test name (map list (list)) '())
(test name (map list '(0)) '((0)))
(test name (map list '(0 1)) '((0) (1)))
(test name (map list '(1 2 3)) '((1) (2) (3)))
(test name (map list '()) '())
(test name (map (lambda (a b) (list b a))
           '(1 2 3 4)
           '(a b c d))
      '((a 1) (b 2) (c 3) (d 4)))
(test name (map (lambda (a b) (list b a))
           '(1 2 3 4)
           '(a b d))
      '((a 1) (b 2) (d 3)))

;; Modulo
(testq name (modulo 10 6) 4)
(testq name (modulo 10 5) 0)
(testq name (modulo 10 4) 2)
(testq name (modulo 10 3) 1)
(testq name (modulo 10 2) 0)
(testq name (modulo 10 1) 0)
; TODO: Test negative modulo, (modulo 10 -3)

;; integer?
(testq name (integer? 1) #t)
(testq name (integer? 1.0) #t)
(testq name (integer? 1.1) #f)
(testq name (integer? (quote b)) #f)

;; Conversion
(testq name (integer->char 65) (quote #\A))
(testq name (integer->char 97) (quote #\a))
(testq name (list->string (list #\a #\b #\c)) "abc")
(testq name (list->string (list #\a #\b #\b #\A)) "abbA")
(test name (list-tail (list 1 2 3) 0) (list 1 2 3))
(test name (list-tail (list 1 2 3) 1) (list 2 3))
(test name (list-tail (list 1 2 3) 2) (list 3))
(test name (list-tail (list 1 2 3) 3) (list))

;; member
(testq name (member 10 (list 1 2 3)) #f)
(test name (member 10 (list 10 20 30)) (list 10 20 30))
(test name (member 20 (list 10 20 30)) (list 20 30))
(test name (member 30 (list 10 20 30)) (list 30))
(testq name (member 40 (list 10 20 30)) #f)
(test name (member 20
               (list 10 20 30
                     (quote bee)
                     (quote cee)))
       (list 20 30 (quote bee) (quote cee)))


;; memv (TODO: insert eqv? specific check)
(testq name (memv 10 (list 1 2 3)) #f)
(test name (memv 10 (list 10 20 30)) (list 10 20 30))
(test name (memv 20 (list 10 20 30)) (list 20 30))
(test name (memv 30 (list 10 20 30)) (list 30))
(testq name (memv 40 (list 10 20 30)) #f)
(test name (memv 20 (list 10 20 30
                      (quote bee)
                      (quote cee)))
       (list 20 30 (quote bee) (quote cee)))

;; memq (TODO: insert eq? specific check)
(testq name (memq 10 (list 1 2 3)) #f)
(test name (memq 10 (list 10 20 30)) (list 10 20 30))
(test name (memq 20 (list 10 20 30)) (list 20 30))
(test name (memq 30 (list 10 20 30)) (list 30))
(testq name (memq 40 (list 10 20 30)) #f)
(test name (memq 20 (list 10 20 30
                      (quote bee) (quote cee)))
       (list 20 30 (quote bee) (quote cee)))

;; gcd, lcm
(testq name (gcd) 0)
(testq name (gcd 10) 10)
(testq name (gcd -10) 10)
(testq name (gcd 10 2) 2)
(testq name (gcd 10 3) 1)
(testq name (gcd 10 5) 5)
(testq name (gcd 5 10) 5)
(testq name (gcd 32 -36) 4)
(testq name (gcd -32 36) 4)
(testq name (gcd -32 -36) 4)
(testq name (gcd 4 6 8 10) 2)
(testq name (gcd 4 6 8 12) 2)
(testq name (gcd 40 24) 8)
(testq name (gcd 1230 4560) 30)
(testq name (lcm) 1)
(testq name (lcm 10) 10)
(testq name (lcm -10) 10)
(testq name (lcm 10 10) 10)
(testq name (lcm 4 6) 12)
(testq name (lcm 6 4) 12)
(testq name (lcm 2 4 6) 12)
(testq name (lcm 2 4 6 8) 24)
(testq name (lcm 2 4 6 -8) 24)
(testq name (lcm 2 -4 6 -8) 24)
(testq name (lcm 201 202 203) 8242206)
(testq name (lcm 201 202 203 204) 280235004)

;; list-ref
(testq name (list-ref (list 1 2 3) 2) 3)
(testq name (list-ref (list 1 2 3) 1) 2)
(testq name (list-ref (list 1 2 3) 0) 1)
(testq name (list-ref (list 1 2 3 4) 0) 1)
(testq name (list-ref (list 1 2 3 4) 3) 4)

;; string conversion
(test name (string->list "hey") (list #\h #\e #\y))
(testq name (string->symbol "hey") (quote hey))
(testq name (string->number "abba") #f)
(testq name (string->number "123") 123)
(testq name (string->number "456") 456)
(testq name (string->number "1.2") 1.2)
(testq name (string->number "1.5") 1.5)
(testq name (string-length "abba") 4)
(testq name (string-length "abb") 3)
(testq name (string-length "ab") 2)
(testq name (string-length "a") 1)
(testq name (string-length "") 0)
(testq name (substring "Hello!" 0 0) "")
(testq name (substring "Hello!" 0 1) "H")
(testq name (substring "Hello!" 0 3) "Hel")
(testq name (substring "Hello!" 1 3) "ell")
(testq name (substring "Hello!" 1 3) "ell")
(testq name (substring "Hello!" 1 4) "ello")
(testq name (substring "Hello!" 2 4) "llo!")
(testq name (string=? "hey" "hey") #t)
(testq name (string=? "hey" "heya") #f)
(testq name (string<=? "hey" "heya") #t)
(testq name (string-ref "hey" 0) #\h)
(testq name (string-ref "hey" 1) #\e)
(testq name (string-ref "hey" 2) #\y)

;; append tests
(testq name (append (list) 1) 1)
(test name (append (list) (list 1 2)) (list 1 2))
(test name (append (list) (list 1 2)) (list 1 2))
(test name (append (list 1)) (list 1))
(test name (append (list 1)) (list 1))
(test name (append (list 1) 2) (cons 1 2))
(test name (append (list 1) (list 3)) (list 1 3))
(test name (append (list 1) (list 3 4)) (list 1 3 4))
(test name (append (list 1) (list 3 4) 5) (cons 1 (cons 3 (cons 4 5))))
(test name (append
         (append
           (list 1 2 3)
           (list 4))
         (list 5 6)
           7)
       (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))

;; Regression test: append should not mutate
(test name
  (begin
    (define a (list 1))
    (append a 2)
    (append a 3)
    a) (list 1))

;; Quoting
(test name 'hey (quote hey))
(test name 'hey 'hey)
(test name ''hey ''hey)
(test name (append '(1 2 3) 4) (cons 1 (cons 2 (cons 3 4))))
(testq name (apply + '(1 2 3)) 6)
(testq name (apply + '(1 2 3)) 6) ; is double-quote ok?
;
(test name (quasiquote (1 2 (unquote (+ 3 4)))) '(1 2 7)) ; this test name should work nicely (TODO)
(test name (quasiquote (1 2 (unquote (+ 3 4)))) (list 1 2 7)) ; this test name should work nicely (TODO)
(test name `(1 2 ,(+ 3 4) 3 y) '(1 2 7 3 y))
;
; Quasiquoting with unquote-splicing ",@"
;(testq name (quote (define foo '(11 22 33)) `(1 2 3 foo ,foo ,@foo)) (quote (1 2 3 foo (11 22 33) 11 22 33)))
(test name `(1 2 ,@(list 3 4 5) 6 7) (list 1 2 3 4 5 6 7))
(test name (let ((a '(11 22 33))) `(+ ,@a 44)) '(+ 11 22 33 44))

;; Number system
(testq name (real? 3) #t) ; from r7rs draft

;; Characters
(testq name (string-length (make-string 0)) 0)
(testq name (string-length (make-string 1)) 1)
(testq name (string-length (make-string 2)) 2)
(testq name (string-length (make-string 42)) 42)
(testq name (string-length (make-string 42 #\a)) 42)
(testq name (string-length (make-string 42 #\0)) 42)
(test name (make-string 1 #\x28) "(")
(test name (make-string 2 #\x28) "((")
(test name (make-string 3 #\x28) "(((")
(test name (make-string 3 #\x29) ")))")
(test name (make-string 5 #\a) "aaaaa")
(test name (make-string 5 #\A) "AAAAA")
(test name (make-string 7 #\space) "       ")

;; TODO/BUG: Here's another one.  When parsing "#\newline", it is converted
;; to char and then displayed by sprint() / print() as a newline, instead of
;; the literal "#\newline". Fix that.
(testq name (string-length (make-string 9 #\newline)) 9)
(testq name (string-length (make-string 7 #\tab)) 7)
(testq name (string-length (make-string 3 #\x1)) 3)

;; The following two tests are BUGs in our implementation, because we use zero
;; terminated strings internally for the STRING type, and therefore
;; cannot count using strlen().  Either switch to std::string or reimplement
;; STRING.
(xfailq name (string-length (make-string 3 #\x0)) 3 "TODO: Add embedded null chars.")
(xfailq name (string-length (make-string 3 #\null)) 3 "TODO: Add embedded null chars.")

;; Special character literals
(testq name (length (list #\( #\))) 2)
(testq name (vector-length #(1 2 3 #\))) 4)

;; Comment-out datum
(testq name (+ 1 #; 2 3) 4)
(testq name (+ 1 #;2 3) 4)

;; Quasiquote
(test name `(1 2 ,(+ 3 4)) '(1 2 7))
(testq name '() (list))
(testq name '() '())
(testq name (list) '())
(testq name (list) '())

;; Test "#;", or "Ignore next form":
(test name `(a #;(this should be ignored) b c) '(a b c))
(test name `(a #; (this should also be ignored) b c) '(a b c))
(test name `(a #; zz b c) '(a b c))

;; Vectors
(testq name (vector-length (vector 1 2 3)) 3)
(testq name (vector-length (vector)) 0)
(testq name (vector-length (vector 1 2 foo)) 3) ; self-evaluating elements
(testq name
  (vector-length
    (vector 1 2 (define (v foo) (* foo foo)) 100)) 4)

(testq name (list? (cons 1 2)) #f)
(testq name (list? (quote (a . b))) #f)
(testq name (pair? (cons 1 2)) #t)
(testq name (pair? (quote (a . b))) #t)
(testq name (length (list '() '())) 2)

; to fix below bugs, tokenizer should must return 3 tokens for "a""b""c"
(testq name (length '(#;"a""b""c")) 2)
(testq name (length '("a"#;"b""c""d""e")) 4)

; to fix, enable support for \x<hex-number> escapes in string parser
(testq name (string-length "a\x42;c") 3)
(testq name "a\x42;c" "aBc")

(test name (map (lambda (x) (* x x)) '(0 1 2 3 4 5)) '(0 1 4 9 16 25))

;; Exactness
(testq name (exact? 3.4) #f)
(testq name (exact? 10.0) #f)
(testq name (exact? (* 2 2)) #t)
(testq name (exact? (* 2 2.0)) #f)
(testq name (exact? (+ 1 2 3)) #t)
(testq name (exact? (+ 1 2 3 (* 10 20))) #t)
(testq name (exact? (+ 1 2 3 (* 10 20 4))) #t)
(testq name (exact? (+ 1 2 3 (* 10 20 4 1))) #t)
(testq name (exact? (+ 1 2 3 (* 10 20 4 1.0))) #f)
(testq name (exact? (exact 1)) #t)
(testq name (exact? (exact 1.1)) #t)
(testq name (exact? (exact -1.1)) #t)
(testq name (exact? (exact -1)) #t)
(testq name (exact? (exact 0)) #t)
(testq name (exact? #e123) #t)
(testq name (inexact? #e123) #f)
(testq name (exact? #i123) #f)
(testq name (inexact? #i123) #t)
(testq name (exact? #e123.456) #t)
(testq name (inexact? #e123.456) #f)
(testq name (inexact? #i123.456) #t)
(testq name (exact? #i123.456) #f)
(test name (exact 10) 10)
(test name (exact 10.2) 51/5)
(test name (exact 0.1) 1/10)
(test name (exact 0.2) 2/10)
(test name (exact 0.02) 2/100)
(test name (exact 0.002) 2/1000)
(test name (exact 1.1) 11/10)
(test name (exact 1.2) 6/5)
(test name (equal? (inexact 10/2) (inexact 5)) #t)
(test name (inexact 10/2) (inexact 5))
(test name (inexact 10/2) 5.0)
(test name (inexact 1/2) 0.5)
(test name (inexact 1/2) .5)
(test name (inexact? (inexact 5)) #t)
(test name (inexact 10/3) 3.33333)
(test name (exact 123.456) 15432/125)
(test name (exact .1) 1/10)
(test name (exact .01) 1/100)
(test name (exact .001) 1/1000)
(test name (exact .0001) 1/10000)
(test name (exact .00001) 1/100000)
(test name (exact .000001) 1/1000000)
(test name (exact .0000001) 1/10000000)
(test name (exact .0000002) 1/5000000)
(test name (exact .0000003) 3/10000000)

;; Empty list and open-output-string
;; (note, we don't support open-output-string yet)
;(test name (let ((s (open-output-string)))
;        (display '(lambda () 1 2) s)
;        (get-output-string s)) "(lambda () 1 2)")
(test name (cons 1 '()) '(1))
(test name (cons 1 '()) (list 1))
(test name (cons 2 (cons 1 (list))) '(2 1))
(test name (cons 2 (cons 1 (list))) (list 2 1))
(test name (cons 2 (cons 1 '())) '(2 1))
(test name (cons 2 (cons 1 '())) (list 2 1))
(test name (length (cons 1 '())) 1)
(test name (length (cons 2 (cons 1 '()))) 2)
(test name (length (list '())) 1)
(test name (length (list '() '())) 2)
(test name (pair? '()) #f)
(test name (pair? '(())) #t)
(test name (pair? (list (list))) #t)
(test name (pair? (list)) #f)
(test name (null? '()) #t)
(test name (null? '(())) #f)

;; Subtraction errors
(test name (exact? (- 1 1)) #t)
(test name (integer? (- 1 1)) #t)
(test name (integer? (- 10 10)) #t)
(test name (integer? (- 1 1)) #t)
(test name (integer? (- 3 1 1 1)) #t)
(test name (- 1 1) #e0)
(test name (- 3 1 1 1) 0)
(test name (integer? (- 3 1 1 1)) #t)

;; Tokenizer should recognize "a""b" as two tokens "a" and "b"
(test name '("1""2""3") (list "1" "2" "3"))
(test name '("foo""bar""baz") (list "foo" "bar" "baz"))
(test name (length '("foo""bar""baz")) 3)
;; Escaped characters should still work
(test name (string-length "Say: \"Hello!\".") 14)
(test name (length (list "Backslash: \\""Another string")) 2)
(test name (length (list "Backslash: \\""Another string")) 2)
(test name (string-ref (car (list "Backslash: \\""Another string")) 11) #\x5c)

;; Symbols enclosed in pipes, format "|a b c|"
(test name (let ((|a b| 123)) |a b|) 123)
(test name (let ((|a  b | 123) (|c d e| 876)) (+ |a  b | |c d e|)) 999)
(test name (symbol->string '|foo bar  baz |) "|foo bar  baz |")
(test name (let ((|squared number| (lambda (x) (* x x))))
        (|squared number| 12)) 144)

;; Call-with-values
(test name (call-with-values
        (lambda () (values -11 1880))
        (lambda (a b) (+ (* a a) b))) 2001)

;; define-macro, aka defmacro, tests
(let ()
  (define-macro defmacro-foo1 (a b)
    ;; basic pattern, ",a" means "EVALUATE argument a",
    ;; while "a" means "just use a as-is"
    `(+ ,a (* ,b 10)))
  (testq name (defmacro-foo1 3 2) 23)
  (testq name (defmacro-foo1 (+ 1 2) 2) 23))

;; Note on above: if we did `(+ a (,b * 10))
;; without using ",a" then it would try to evaluate
;; (+ <a unevaluated> (* <value of b> 10)).
;; TODO: Add a test name for this.

;; Other tests
(test name (make-list 3 2) '(2 2 2))
(test name (make-list 3) '(#f #f #f))
(test name (make-list 2 '(a b)) '((a b) (a b)))
