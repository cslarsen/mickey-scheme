#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (test unit-testint)
  (import (scheme base))
  (export
    testq
    testv
    test
    result)
  (begin
    (define total-tests 0)
    (define failed-tests 0)

    (define-syntax perform-test
      (syntax-rules ()
        ((_ eq-op code expected)
         (let*
           ((actual-result code)
            (success (eq-op actual-result expected)))
           (set! total-tests (+ total-tests 1))
           (set! failed-tests (+ failed-tests
                                 (if success 0 1)))
           (display (string-append
             (number->string total-tests) " "
             (if success "OK" "FAIL") ": "))
           (display (quote code))

           (if (not success)
             (begin
               (display " != ")
               (display expected)))

           (display " ==> ")
           (display actual-result)
           (newline)))))

    (define-syntax testq
      (syntax-rules ()
        ((_ code expected)
         (perform-test eq? code expected))))

    (define-syntax testv
      (syntax-rules ()
        ((_ code expected)
         (perform-test eqv? code expected))))

    (define-syntax test
      (syntax-rules ()
        ((_ code expected)
         (perform-test equal? code expected))))

    (define (result)
      (list (list 'total total-tests)
            (list 'good (- total-tests failed-tests))
            (list 'fail failed-tests)))))
