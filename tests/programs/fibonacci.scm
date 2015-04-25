(import
  (scheme base)
  (scheme write)
  (test unit-test))

(define name "fibonacci.scm")

;; Slow, recursive version of Fibonacci sequence.
(define (fib-slow n)
  (if (<= n 1) n
      (+ (fib-slow (- n 1))
         (fib-slow (- n 2)))))

(testq name (fib-slow 0) 0)
(testq name (fib-slow 1) 1)
(testq name (fib-slow 2) 1)
(testq name (fib-slow 3) 2)
(testq name (fib-slow 4) 3)
(testq name (fib-slow 5) 5)
(testq name (fib-slow 6) 8)
(testq name (fib-slow 7) 13)
(testq name (fib-slow 8) 21)
(testq name (fib-slow 9) 34)
(testq name (fib-slow 10) 55)
(testq name (fib-slow 11) 89)
(testq name (fib-slow 12) 144)
