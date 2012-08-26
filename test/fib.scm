(import (scheme base))
(import (scheme write))
;; Fibonacci sequence, slow, recursive version

(define (<= a b)
  (or (< a b) (= a b)))

(define (fib n)
  (if (<= n 1) n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (show-fib n)
  (display (string-append
    "fib(" (number->string n) ") = " (number->string (fib n)) "\n")))

(show-fib 0)
(show-fib 1)
(show-fib 2)
(show-fib 3)
(show-fib 4)
(show-fib 5)
(show-fib 6)
(show-fib 7)
(show-fib 8)
(show-fib 9)
(show-fib 10)
(show-fib 20)
(show-fib 24)
