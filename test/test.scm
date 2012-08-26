;; Library for performing tests
(define tests 0)
(define test+1
  (lambda (_foo)
    (set! tests (+ tests 1))))

(define failed 0)
(define fail+1
  (lambda (_foo)
    (set! failed (+ failed 1))))

(define (fail code expected actual)
  (test+1 (quote _))
  (fail+1 (quote _))
  (display tests) (display " FAIL: ")
  (display code) (display " != ") (display expected)
  ; show actual result
  (display " ==> ") (display actual) (newline))

(define (success code expected)
  (test+1 (quote _))
  (display tests) (display " OK: ")
  (display code) (display " ==> ") (display expected) (newline))

(define (test-eq code expected)
  (let ((result (eval code)))
    (if (equal? result expected)
      (success code expected)
      (fail code expected result))))

(define (results)
  (display 
    (string-append
      (number->string (- tests failed)) " / " (number->string tests) " tests OK, "
        (number->string failed) " failed\n")))
