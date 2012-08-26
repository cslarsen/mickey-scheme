(import (scheme load)
        (scheme cxr)
        (scheme base)
        (scheme write)
        (scheme char)
        (scheme math)
        (scheme char)
        (mickey environment))

;;
;; Define some test functions
;;

(display "Loading complex.scm ... ")
(load "complex.scm")

(display "Loading test.scm ... ")
(load "test.scm")

(display "DONE")
(newline)

;;
;; Perform actual tests
;;

(display "Tests\n")
(test-eq (quote (+ 1 2 3 4)) 10)
(test-eq (quote (* 1 2 3 4)) 24)
(test-eq (quote (- 1 2 3 4)) -8)
(test-eq (quote (string-append "a" "b" "b" "a")) "abba")
(test-eq (quote (number->string 12345)) "12345")
;(test-eq (quote (boolean->string #f)) "#f")
;(test-eq (quote (boolean->string #t)) "#t")
;(test-eq (quote (boolean->string (eq? 1 2))) "#f")
;(test-eq (quote (boolean->string (eq? 3 (+ 1 2)))) "#t")
;(test-eq (quote (boolean->string (eq? 3 (+ 1 2 3)))) "#f")
(test-eq (quote (list 1 2 (list 3 4))) (list 1 2 (list 3 4)))
(test-eq (quote (apply + (list 1 2 3))) 6)
(test-eq (quote (apply + (quote (1 2 3)))) 6)
(test-eq (quote (apply + (list 1 2 3 (* 5 6)))) 36)
(test-eq (quote (apply + (list 1 2 3 (* 5 5)))) 31)
(test-eq (quote (complex->string
                  (*complex (make-complex 2 3)
                            (make-complex 5 7)))) "-11 + 29i")
(test-eq (quote (- 0.5 -1.5)) 2)
(test-eq (quote (- 0.5 -10.5)) 11)
(test-eq (quote (- 1 2 3)) -4)
(test-eq (quote (- 1)) -1)
(test-eq (quote (- 2)) -2)
(test-eq (quote (- -2)) 2)

;; pair?, list? and dot notation
(test-eq (quote '(1 . 2)) (cons 1 2))
(test-eq (quote (pair? '(1 . 3))) #t)
(test-eq (quote (pair? (cons 1 3))) #t)
(test-eq (quote (list? '(1 . 3))) #f)
(test-eq (quote (list? (cons 1 3))) #f)
(test-eq (quote (pair? (list 1 2))) #t)
(test-eq (quote (list? (list 1 2))) #t)
(test-eq (quote (pair? '())) #f)
(test-eq (quote (list? '())) #t)
(test-eq (quote (list? 123)) #f)
(test-eq (quote (pair? 123)) #f)
(test-eq (quote (+ 1 . (2 . (3 . ())))) 6)
(test-eq (quote (list? '(1 . 2))) #f)

;; (if t a1 a2)
(test-eq (quote (if (> 4 2) 11 22)) 11)
(test-eq (quote (if (> 3 2) 11 22)) 11)
(test-eq (quote (if (> 2 2) 11 22)) 22)
(test-eq (quote (if (> 1 2) 11 22)) 22)

;; (if t a1)
(test-eq (quote (if (> 3 2) 11)) 11)
(test-eq (quote (if (< 1 2) 11)) 11)

;; (and)
(test-eq '(and (= 2 2) (> 2 1)) #t)
(test-eq '(and (= 2 2) (< 2 1)) #f)
(test-eq '(and 1 2 'c '(f g)) '(f g))
(test-eq '(and) #t)
(test-eq '(and 1) 1)
(test-eq '(and 0) 0)
(test-eq '(and 0 #f) #f)
(test-eq '(and 1 #f 2) #f)
(test-eq '(and 1 2 (quote c) (quote (f g))) '(f g))

;; (or)
(test-eq '(or (= 2 2) (> 2 1)) #t)
(test-eq '(or (= 2 2) (< 2 1)) #t)
(test-eq '(or #f #f #f) #f)
(test-eq '(or #t #f #f) #t)
(test-eq '(or #f #t #f) #t)
(test-eq '(or #f #f #t) #t)
(test-eq '(or (memq 'b '(a b c)) (/ 3 0)) '(b c))
(test-eq '(or) #f)
(test-eq '(or #t (/ 1 0)) #t)
(test-eq '(or 10 (/ 1 0)) 10)
(test-eq '(or (quote (a b c)) (/ 1 0)) '(a b c))

;; (lambda)
(test-eq '((lambda x x) 3 4 5 6) '(3 4 5 6))
(test-eq '((lambda (x y . z) z) 3 4 5 6) '(5 6))

;; (reverse ...)
(test-eq (quote (reverse (list 1 2 3 4))) (list 4 3 2 1))
(test-eq (quote (reverse (list 1 2 3))) (list 3 2 1))
(test-eq (quote (reverse (list 1 2))) (list 2 1))
(test-eq (quote (reverse (list 1))) (list 1))

;; (abs <int>)
(test-eq (quote (abs -2)) 2)
(test-eq (quote (abs -1)) 1)
(test-eq (quote (abs 0)) 0)
(test-eq (quote (abs 1)) 1)
(test-eq (quote (abs 2)) 2)
;; (abs <float>)
(test-eq (quote (abs -2.1)) 2.1)
(test-eq (quote (abs -1.1)) 1.1)
(test-eq (quote (abs 0.1)) 0.1)
(test-eq (quote (abs -0.1)) 0.1)
(test-eq (quote (abs 1.1)) 1.1)
(test-eq (quote (abs 2.1)) 2.1)

;; car and cdr
(test-eq (quote (caddr '(1 2 3 4))) 3)
(test-eq (quote (cdddr '(1 2 3 4 5))) '(4 5))
(test-eq (quote (cadddr '(1 2 3 4 5))) 4)

;; set! and friends
(test-eq
  (quote
    (begin
      (set-cdr! (list 1 2) 3)
      123)) 123) ;; just check that set-cdr! will accept
                 ;; a non-symbol
(test-eq
  (quote
    (let ((v (list 1 2)))
      (set-cdr! v 3)
      v))
  (cons 1 3))
(test-eq
  (quote
    (begin
      (set-car! (list 1 2) 3)
      123)) 123) ;; just check that set-car! will accept
                 ;; a non-symbol
(test-eq
  (quote
    (let ((v (list 1 2)))
      (set-car! v 3)
      v))
  '(3 2))

;; assq
; TODO: Have to make test-eq a macro
;(test-eq (quote (assq (quote two) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote two) 2)))
;(test-eq (quote (assq (quote one) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (quote (list (quote one) 1)))
(test-eq (quote (assq (quote three) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (list (quote three) 3))
(test-eq (quote (assq (quote two) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) (list (quote two) 2))
(test-eq (quote (assq (quote threee) (list (list (quote one) 1) (list (quote two) 2) (list (quote three) 3)))) #f)

;; even, odd
(test-eq (quote (even? 0)) #t) ;; correct in MIT Scheme
(test-eq (quote (even? 1)) #f)
(test-eq (quote (even? 2)) #t)
(test-eq (quote (even? 3)) #f)
(test-eq (quote (even? 4)) #t)
(test-eq (quote (even? 9)) #f)
(test-eq (quote (even? 100)) #t)
(test-eq (quote (even? 1000)) #t)
(test-eq (quote (odd? 0)) #f) ;; correct in MIT Scheme
(test-eq (quote (odd? 1)) #t)
(test-eq (quote (odd? 2)) #f)
(test-eq (quote (odd? 3)) #t)
(test-eq (quote (odd? 4)) #f)
(test-eq (quote (odd? 9)) #t)
(test-eq (quote (odd? 100)) #f)
(test-eq (quote (odd? 1000)) #f)

;; positive, negative
(test-eq (quote (negative? 0)) #f)
(test-eq (quote (negative? 1)) #f)
(test-eq (quote (negative? 2)) #f)
(test-eq (quote (negative? 3)) #f)
(test-eq (quote (negative? -1)) #t)
(test-eq (quote (negative? -2)) #t)
(test-eq (quote (negative? -3)) #t)
;;
(test-eq (quote (positive? 0)) #f)
(test-eq (quote (positive? 1)) #t)
(test-eq (quote (positive? 2)) #t)
(test-eq (quote (positive? 3)) #t)
(test-eq (quote (positive? -1)) #f)
(test-eq (quote (positive? -2)) #f)
(test-eq (quote (positive? -3)) #f)

;; eqv? tests from R7RS, section 6.1
(test-eq (quote (eqv? (quote a) (quote a))) #t) ;; (eqv? 'a 'a)
(test-eq (quote (eqv? (quote a) (quote b))) #f)
(test-eq (quote (eqv? 2 2)) #t)
(test-eq (quote (eqv? 2 1)) #f)
(test-eq (quote (eqv? (list) (list))) #t) ;; (eqv? '() '())
(test-eq (quote (eqv? 100000000 100000000)) #t)
(test-eq (quote (eqv? (cons 1 2) (cons 1 2))) #f)
(test-eq (quote (eqv? (lambda () 1) (lambda () 2))) #f)
(test-eq (quote (eqv? #f (quote nil))) #f)
(test-eq (quote (let ((p (lambda (x) x)))
                  (eqv? p p))) #t)

;; unspecified tests for eqv
; (eqv? "" "")
; (eqv? '#() '#())
; (eqv? (lambda (x) x) (lambda (x) x))
; (eqv? (lambda (x) x) (lambda (y) y))

;; round
(test-eq (quote (round 1)) 1)
(test-eq (quote (round 2)) 2)
(test-eq (quote (round 0.9)) 1.0)
(test-eq (quote (round 1.0)) 1.0)
(test-eq (quote (round 1.1)) 1.0)
(test-eq (quote (round 1.2)) 1.0)
(test-eq (quote (round 1.3)) 1.0)
(test-eq (quote (round 1.4)) 1.0)
(test-eq (quote (round 1.5)) 2.0)
(test-eq (quote (round 1.6)) 2.0)
(test-eq (quote (round 2.49)) 2.0)
(test-eq (quote (round 2.50)) 3.0) ;; NOTE: Chicken and MIT Scheme reports 2! IEEE-754 magic or?
(test-eq (quote (round 2.51)) 3.0)

;; truncate
(test-eq (quote (truncate 1.1)) 1.0)
(test-eq (quote (truncate 1.4)) 1.0)
(test-eq (quote (truncate 1.5)) 1.0)
(test-eq (quote (truncate 1.6)) 1.0)
(test-eq (quote (truncate 1.9)) 1.0)
(test-eq (quote (truncate 1)) 1)
(test-eq (quote (truncate 2)) 2)

;; min
(test-eq (quote (min 1 2 3)) 1)
(test-eq (quote (min 4 2 3)) 2)
(test-eq (quote (min 4.2 2 3)) 2)
(test-eq (quote (min 4.2 2.3 3)) 2.3)
(test-eq (quote (min 4.2)) 4.2)

;; max
(test-eq (quote (max 1 2 3)) 3)
(test-eq (quote (max 4 2 3)) 4)
(test-eq (quote (max 4.2 2 3)) 4.2)
(test-eq (quote (max 4.2 2.3 3)) 4.2)
(test-eq (quote (max 4.2)) 4.2)

;; expt
(test-eq (quote (expt 2 0)) 1)
(test-eq (quote (expt 2 1)) 2)
(test-eq (quote (expt 2 2)) 4)
(test-eq (quote (expt 2 3)) 8)
(test-eq (quote (expt 3 3)) 27)

;; char-whitespace
(test-eq (quote (char-whitespace? #\a)) #f)
(test-eq (quote (char-whitespace? #\b)) #f)
(test-eq (quote (char-whitespace? #\c)) #f)
; TODO: Test for #\tab and friends

;; other char tests
(test-eq (quote (char-upcase #\a)) #\A)
(test-eq (quote (char-upcase #\A)) #\A)
(test-eq (quote (char-upcase #\h)) #\H)
(test-eq (quote (char-upcase #\z)) #\Z)

(test-eq (quote (char-downcase #\A)) #\a)
(test-eq (quote (char-downcase #\a)) #\a)
(test-eq (quote (char-downcase #\Z)) #\z)

;; The following two string-map tests are from R7RS draft 6:
;;
(test-eq '(string-map char-foldcase "AbdEgH") "abdegh")
;;
(test-eq
  '(string-map
    (lambda (c)
      (integer->char (+ 1 (char->integer c))))
        "HAL") "IBM")
;;
(test-eq
  '(string-map
    (lambda (c k)
      ((if (eqv? k #\u) char-upcase char-downcase)
        c))
    "studlycaps xxx"
    "ululululul") "StUdLyCaPs")

;; Taken from R7RS draft 6
;;
(test-eq (quote
  (let ((v '()))
    (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
        "abcde")
        v)) '(101 100 99 98 97))

;; N input strings requires N parameters to the lambda:
(test-eq (quote
  (let ((v '()))
    (string-for-each
      (lambda (one two three)
        (set! v (cons three (cons one v))))
      "ab" "cd" "efg") v))
  '(f b e a))

;; modulo
(test-eq (quote (modulo 10 6)) 4)
(test-eq (quote (modulo 10 5)) 0)
(test-eq (quote (modulo 10 4)) 2)
(test-eq (quote (modulo 10 3)) 1)
(test-eq (quote (modulo 10 2)) 0)
(test-eq (quote (modulo 10 1)) 0)
; TODO: Test negative modulo, (modulo 10 -3)

;; integer?
(test-eq '(integer? 1) #t)
(test-eq '(integer? 1.0) #t)
(test-eq '(integer? 1.1) #f)
(test-eq '(integer? (quote b)) #f)

;; char functions
(test-eq (quote (char->integer #\a)) 97)
(test-eq (quote (char->integer #\b)) 98)
(test-eq (quote (char->integer #\A)) 65)
;;
(test-eq (quote (char-alphabetic? #\a)) #t)
(test-eq (quote (char-alphabetic? #\A)) #t)
(test-eq (quote (char-alphabetic? #\2)) #f)
(test-eq (quote (char-alphabetic? #\8)) #f)
;;
(test-eq (quote (char-lower-case? #\8)) #f)
(test-eq (quote (char-lower-case? #\Z)) #f)
(test-eq (quote (char-lower-case? #\A)) #f)
(test-eq (quote (char-lower-case? #\H)) #f)
(test-eq (quote (char-lower-case? #\z)) #t)
(test-eq (quote (char-lower-case? #\a)) #t)
(test-eq (quote (char-lower-case? #\h)) #t)
;;
(test-eq (quote (char-upper-case? #\8)) #f)
(test-eq (quote (char-upper-case? #\Z)) #t)
(test-eq (quote (char-upper-case? #\A)) #t)
(test-eq (quote (char-upper-case? #\H)) #t)
(test-eq (quote (char-upper-case? #\z)) #f)
(test-eq (quote (char-upper-case? #\a)) #f)
(test-eq (quote (char-upper-case? #\h)) #f)
;;
(test-eq (quote (char-numeric? #\8)) #t)
(test-eq (quote (char-numeric? #\Z)) #f)
(test-eq (quote (char-numeric? #\A)) #f)
(test-eq (quote (char-numeric? #\a)) #f)
(test-eq (quote (char-numeric? #\0)) #t)
(test-eq (quote (char-numeric? #\1)) #t)
(test-eq (quote (char-numeric? #\9)) #t)
;;
(test-eq (quote (char<=? #\9 #\0)) #f)
(test-eq (quote (char<=? #\3 #\5)) #t)
(test-eq (quote (char<=? #\a #\z)) #t)
(test-eq (quote (char<=? #\a #\a)) #t)
(test-eq (quote (char<=? #\h #\e)) #f)
(test-eq (quote (char<=? #\r #\w)) #t)
;;
(test-eq (quote (char>=? #\9 #\0)) #t)
(test-eq (quote (char>=? #\3 #\5)) #f)
(test-eq (quote (char>=? #\a #\z)) #f)
(test-eq (quote (char>=? #\a #\a)) #t)
(test-eq (quote (char>=? #\h #\e)) #t)
(test-eq (quote (char>=? #\r #\w)) #f)
;;
(test-eq (quote (char>? #\9 #\0)) #t)
(test-eq (quote (char>? #\3 #\5)) #f)
(test-eq (quote (char>? #\a #\a)) #f)
(test-eq (quote (char>? #\h #\e)) #t)
(test-eq (quote (char>? #\r #\w)) #f)
;;
(test-eq (quote (char=? #\9 #\0)) #f)
(test-eq (quote (char=? #\3 #\5)) #f)
(test-eq (quote (char=? #\a #\a)) #t)
(test-eq (quote (char=? #\4 #\4)) #t)
(test-eq (quote (char=? #\h #\e)) #f)
(test-eq (quote (char=? #\r #\w)) #f)

;; conversion
(test-eq (quote (integer->char 65)) (quote #\A))
(test-eq (quote (integer->char 97)) (quote #\a))
(test-eq (quote (list->string (list #\a #\b #\c))) "abc")
(test-eq (quote (list->string (list #\a #\b #\b #\A))) "abbA")

(test-eq (quote (list-tail (list 1 2 3) 0)) (list 1 2 3))
(test-eq (quote (list-tail (list 1 2 3) 1)) (list 2 3))
(test-eq (quote (list-tail (list 1 2 3) 2)) (list 3))
(test-eq (quote (list-tail (list 1 2 3) 3)) (list))

;; member
(test-eq (quote (member 10 (list 1 2 3))) #f)
(test-eq (quote (member 10 (list 10 20 30))) (list 10 20 30))
(test-eq (quote (member 20 (list 10 20 30))) (list 20 30))
(test-eq (quote (member 20 (list 10 20 30 (quote bee) (quote cee)))) (list 20 30 (quote bee) (quote cee)))
(test-eq (quote (member 30 (list 10 20 30))) (list 30))
(test-eq (quote (member 40 (list 10 20 30))) #f)

;; memv (TODO: insert eqv? specific check)
(test-eq (quote (memv 10 (list 1 2 3))) #f)
(test-eq (quote (memv 10 (list 10 20 30))) (list 10 20 30))
(test-eq (quote (memv 20 (list 10 20 30))) (list 20 30))
(test-eq (quote (memv 20 (list 10 20 30 (quote bee) (quote cee)))) (list 20 30 (quote bee) (quote cee)))
(test-eq (quote (memv 30 (list 10 20 30))) (list 30))
(test-eq (quote (memv 40 (list 10 20 30))) #f)

;; memq (TODO: insert eq? specific check)
(test-eq (quote (memq 10 (list 1 2 3))) #f)
(test-eq (quote (memq 10 (list 10 20 30))) (list 10 20 30))
(test-eq (quote (memq 20 (list 10 20 30))) (list 20 30))
(test-eq (quote (memq 20 (list 10 20 30 (quote bee) (quote cee)))) (list 20 30 (quote bee) (quote cee)))
(test-eq (quote (memq 30 (list 10 20 30))) (list 30))
(test-eq (quote (memq 40 (list 10 20 30))) #f)

;; gcd, lcm
(test-eq (quote (gcd)) 0)
(test-eq (quote (gcd 10)) 10)
(test-eq (quote (gcd -10)) 10)
(test-eq (quote (gcd 10 2)) 2)
(test-eq (quote (gcd 10 3)) 1)
(test-eq (quote (gcd 10 5)) 5)
(test-eq (quote (gcd 5 10)) 5)
(test-eq (quote (gcd 32 -36)) 4)
(test-eq (quote (gcd -32 36)) 4)
(test-eq (quote (gcd -32 -36)) 4)
(test-eq (quote (gcd 4 6 8 10)) 2)
(test-eq (quote (gcd 4 6 8 12)) 2)
(test-eq (quote (gcd 40 24)) 8)
(test-eq (quote (gcd 1230 4560)) 30)
(test-eq (quote (lcm)) 1)
(test-eq (quote (lcm 10)) 10)
(test-eq (quote (lcm -10)) 10)
(test-eq (quote (lcm 10 10)) 10)
(test-eq (quote (lcm 4 6)) 12)
(test-eq (quote (lcm 6 4)) 12)
(test-eq (quote (lcm 2 4 6)) 12)
(test-eq (quote (lcm 2 4 6 8)) 24)
(test-eq (quote (lcm 2 4 6 -8)) 24)
(test-eq (quote (lcm 2 -4 6 -8)) 24)
(test-eq (quote (lcm 201 202 203)) 8242206)
(test-eq (quote (lcm 201 202 203 204)) 280235004)

;; list-ref
(test-eq (quote (list-ref (list 1 2 3) 2)) 3)
(test-eq (quote (list-ref (list 1 2 3) 1)) 2)
(test-eq (quote (list-ref (list 1 2 3) 0)) 1)
(test-eq (quote (list-ref (list 1 2 3 4) 0)) 1)
(test-eq (quote (list-ref (list 1 2 3 4) 3)) 4)

;; string conversion
(test-eq (quote (string->list "hey")) (list #\h #\e #\y))
(test-eq (quote (string->symbol "hey")) (quote hey))
(test-eq (quote (string->number "abba")) #f)
(test-eq (quote (string->number "123")) 123)
(test-eq (quote (string->number "456")) 456)
(test-eq (quote (string->number "1.2")) 1.2)
(test-eq (quote (string->number "1.5")) 1.5)
(test-eq (quote (string-length "abba")) 4)
(test-eq (quote (string-length "abb")) 3)
(test-eq (quote (string-length "ab")) 2)
(test-eq (quote (string-length "a")) 1)
(test-eq (quote (string-length "")) 0)
;;
(test-eq (quote (substring "Hello!" 0 0)) "")
(test-eq (quote (substring "Hello!" 0 1)) "H")
(test-eq (quote (substring "Hello!" 0 3)) "Hel")
(test-eq (quote (substring "Hello!" 1 3)) "ell")
(test-eq (quote (substring "Hello!" 1 3)) "ell")
(test-eq (quote (substring "Hello!" 1 4)) "ello")
(test-eq (quote (substring "Hello!" 2 4)) "llo!")
;;
(test-eq (quote (string=? "hey" "hey")) #t)
(test-eq (quote (string=? "hey" "heya")) #f)
(test-eq (quote (string<=? "hey" "heya")) #t)
;;
(test-eq (quote (string-ref "hey" 0)) #\h)
(test-eq (quote (string-ref "hey" 1)) #\e)
(test-eq (quote (string-ref "hey" 2)) #\y)

;; append tests
(test-eq (quote (append (list) 1)) 1)
(test-eq (quote (append (list) (list 1 2))) (list 1 2))
(test-eq (quote (append (list) (list 1 2))) (list 1 2))
(test-eq (quote (append (list 1))) (list 1))
(test-eq (quote (append (list 1))) (list 1))
(test-eq (quote (append (list 1) 2)) (cons 1 2))
(test-eq (quote (append (list 1) (list 3))) (list 1 3))
(test-eq (quote (append (list 1) (list 3 4))) (list 1 3 4))
(test-eq (quote (append (list 1) (list 3 4) 5)) (cons 1 (cons 3 (cons 4 5))))
(test-eq (quote (append (append (list 1 2 3) (list 4)) (list 5 6) 7)) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))

;; Regression test: append should not mutate
(define a (list 1))
(append a 2)
(append a 3)
(test-eq (quote a) (list 1))

;; Quoting
(test-eq (quote 'hey) (quote hey))
(test-eq (quote 'hey) 'hey)
(test-eq (quote ''hey) ''hey)
;
(test-eq (quote (append '(1 2 3) 4)) (cons 1 (cons 2 (cons 3 4))))
(test-eq (quote (apply + '(1 2 3))) 6)
(test-eq '(apply + '(1 2 3)) 6) ; is double-quote ok?
;
(test-eq (quote (quasiquote (1 2 (unquote (+ 3 4))))) '(1 2 7))
(test-eq (quote `(1 2 ,(+ 3 4) 3 y)) '(1 2 7 3 y))
;
; Quasiquoting with unquote-splicing ",@"
;(test-eq (quote (define foo '(11 22 33)) `(1 2 3 foo ,foo ,@foo)) (quote (1 2 3 foo (11 22 33) 11 22 33)))
(test-eq (quote `(1 2 ,@(list 3 4 5) 6 7)) (list 1 2 3 4 5 6 7))

;; Math module
(test-eq '(ceiling 3.0) 3.0)
(test-eq '(ceiling 3.1) 4.0)
(test-eq '(ceiling 3.4) 4.0)
(test-eq '(ceiling 3.5) 4.0)
(test-eq '(ceiling 3.6) 4.0)
(test-eq '(floor 3.0) 3.0)
(test-eq '(floor 3.1) 3.0)
(test-eq '(floor 3.4) 3.0)
(test-eq '(floor 3.5) 3.0)
(test-eq '(floor 3.6) 3.0)
(test-eq '(floor 3.9) 3.0)
(test-eq '(floor 3.999) 3.0)
(test-eq '(sqrt 3.999) 1.99975)
(test-eq '(sqrt 4.0) 2.0)
(test-eq '(sqrt -4.0) (sqrt -1)) ; nan
(test-eq '(exp 1) 2.71828)
(test-eq '(exp 2) 7.38906)
(test-eq '(atan 45.0) 1.54858)
(test-eq '(atan 12.3) 1.48967)
(test-eq '(acos 1.0) 0.0)
(test-eq '(acos 0.5) 1.0472)
(test-eq '(asin 1.0) 1.5708)
(test-eq '(asin 0.5) 0.523599)
(test-eq '(tan 0.5) 0.546302)
(test-eq '(cos 0.5) 0.877583)
(test-eq '(sin (* 0.5 3.1415926535)) 1.0)
(test-eq '(log 256) 5.54518)
(test-eq '(/ (log 256) (log 2)) 8)
(test-eq '(/ (log 1024) (log 2)) 10)

;; Number system
(test-eq '(real? 3) #t) ; from r7rs draft

;; LLVM JIT
(if (bound? ':llvm:gcd)
  (begin
    (test-eq '(:llvm:gcd (* 11 123) (* 2 11)) 11)
    (test-eq '(:llvm:gcd (* 12 123) (* 2 12)) 12)
    (test-eq '(:llvm:gcd 444 555) (gcd 444 555))))

;; Characters
(test-eq '(string-length (make-string 0)) 0)
(test-eq '(string-length (make-string 1)) 1)
(test-eq '(string-length (make-string 2)) 2)
(test-eq '(string-length (make-string 42)) 42)
(test-eq '(string-length (make-string 42 #\a)) 42)
(test-eq '(string-length (make-string 42 #\0)) 42)
(test-eq '(make-string 1 #\x28) "(")
(test-eq '(make-string 2 #\x28) "((")
(test-eq '(make-string 3 #\x28) "(((")
(test-eq '(make-string 3 #\x29) ")))")
(test-eq '(make-string 5 #\a) "aaaaa")
(test-eq '(make-string 5 #\A) "AAAAA")
(test-eq '(make-string 7 #\space) "       ")

;; TODO/BUG: Here's another one.  When parsing "#\newline", it is converted
;; to char and then displayed by sprint() / print() as a newline, instead of
;; the literal "#\newline". Fix that.
(newline)
(display "The following test shows that sprint() doesn't escape chars");
(newline)
(newline)
(test-eq '(string-length (make-string 9 #\newline)) 9)
(test-eq '(string-length (make-string 7 #\tab)) 7)
(test-eq '(string-length (make-string 3 #\x1)) 3)

;; The following two tests are BUGs in our implementation, because we use zero
;; terminated strings internally for the STRING type, and therefore
;; cannot count using strlen().  Either switch to std::string or reimplement
;; STRING.
(newline)
(display "The following two tests show a bug in the string implementation:")
(newline)
(newline)
(test-eq '(string-length (make-string 3 #\x0)) 3) ; <- TODO/BUG: Fix code!
(test-eq '(string-length (make-string 3 #\null)) 3) ; <- TODO/BUG: Fix code!

;; Comment-out datum
(test-eq '(+ 1 #; 2 3) 4)
(test-eq '(+ 1 #;2 3) 4)

;; Quasiquote
(test-eq (quote `(1 2 ,(+ 3 4))) '(1 2 7))
(test-eq (quote '()) (list)) ;; Note: We should rewrite test-eq as a macro!
(test-eq (quote '()) '())
(test-eq (quote (list)) '())
(test-eq '(list) '())

;; Vectors
(test-eq '(vector-length (vector 1 2 3)) 3)
(test-eq '(vector-length (vector)) 0)
(test-eq '(vector-length (vector 1 2 foo)) 3) ; self-evaluating elements
(test-eq
  '(vector-length
    (vector 1 2 (define (v foo) (* foo foo)) 100)) 4)

;; Enter known bugs here:
;;
;(newline)
;(display "Following are some known bugs")
;(newline)
;(newline)
(test-eq '(list? (cons 1 2)) #f)
(test-eq '(list? (quote (a . b))) #f)
(test-eq '(pair? (cons 1 2)) #t)
(test-eq '(pair? (quote (a . b))) #t)
(test-eq (length (list '() '())) 2)

; to fix below bugs, tokenizer should must return 3 tokens for "a""b""c"
(test-eq (quote (length '(#;"a""b""c"))) 2)
(test-eq (quote (length '("a"#;"b""c""d""e"))) 4)

; to fix, enable support for \x<hex-number> escapes in string parser
(test-eq '(string-length "a\x42;c") 3)
(test-eq "a\x42;c" "aBc")

(display "\nResults\n")
(newline)
(results)

(display "\n")
