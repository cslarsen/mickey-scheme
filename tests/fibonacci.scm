(import
  (scheme base)
  (scheme write)
  (test unit-test))

;; Slow, recursive version of Fibonacci sequence.
(define (fib-slow n)
  (if (<= n 1) n
      (+ (fib-slow (- n 1))
         (fib-slow (- n 2)))))

(testq (fib-slow 0) 0)
(testq (fib-slow 1) 1)
(testq (fib-slow 2) 1)
(testq (fib-slow 3) 2)
(testq (fib-slow 4) 3)
(testq (fib-slow 5) 5)
(testq (fib-slow 6) 8)
(testq (fib-slow 7) 13)
(testq (fib-slow 8) 21)
(testq (fib-slow 9) 34)
(testq (fib-slow 10) 55)
(testq (fib-slow 11) 89)
(testq (fib-slow 12) 144)

(tap-results)
