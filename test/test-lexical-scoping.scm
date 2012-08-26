(import (scheme base))
(import (scheme write))
(import (scheme math))

;; From SICP, section 1.1.8

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

;; Taking advantage of lexical scoping
(define (my-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (show-sqrt x)
  (display (string-append
    "my-sqrt(" (number->string x) ") = "
      (number->string (my-sqrt x))
    " vs builtin sqrt(" (number->string x) ") = "
      (number->string (sqrt x)) "\n")))

(show-sqrt 0)
(show-sqrt 1)
(show-sqrt 2)
(show-sqrt 3)
(show-sqrt 4)
(show-sqrt 5)
(show-sqrt 6)
