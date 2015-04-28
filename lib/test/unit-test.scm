#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (test unit-test)
  (import (scheme base))
  (export
    testq
    xfailq
    testv
    test
    result)
  (begin
    (define total-tests 0)
    (define failed-tests 0)

    (define-syntax perform-test
      (syntax-rules ()
        ((_ eq-op code expected file)
         (let*
           ((actual-result code)
            (success (eq-op actual-result expected)))
           (set! total-tests (+ total-tests 1))
           (set! failed-tests (+ failed-tests
                                 (if success 0 1)))
           (display (string-append
             (if success "PASS" "FAIL") ": "))
           (display file)
           (display " - ")
           (display (quote code))

           (if (not success)
             (begin
               (display " != ")
               (display expected)))

           (display " ==> ")
           (display actual-result)
           (display " ")
           (display total-tests)
           (newline)))))

    (define-syntax perform-test-expect-failure
      (syntax-rules ()
        ((_ eq-op code expected file message)
         (let*
           ((actual-result code)
            (success (eq-op actual-result expected)))
           (set! total-tests (+ total-tests 1))
           (set! failed-tests (+ failed-tests
                                 (if success 0 1)))
           (display (string-append
             (if success "XPASS" "XFAIL") ": "))
           (display file)
           (display " - ")
           (display (quote code))

           (if (not success)
             (begin
               (display " != ")
               (display expected)))

           (display " ==> ")
           (display actual-result)
           (display " ")
           (display total-tests)
           (display " ")
           (display message)
           (newline)))))

    (define-syntax testq
      (syntax-rules ()
        ((_ file code expected)
         (perform-test eq? code expected file))))

    (define-syntax xfailq
      (syntax-rules ()
        ((_ file code expected message)
         (perform-test-expect-failure eq? code expected message file))))

    (define-syntax testv
      (syntax-rules ()
        ((_ file code expected)
         (perform-test eqv? code expected file))))

    (define-syntax test
      (syntax-rules ()
        ((_ file code expected)
         (perform-test equal? code expected file))))

    (define (result)
      (list (list 'total total-tests)
            (list 'good (- total-tests failed-tests))
            (list 'fail failed-tests)))))
