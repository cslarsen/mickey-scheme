#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (test unit-test)
  (import (scheme base))
  (export
    get-test-results
    result
    tap-results
    test
    testq
    testv
    xfailq)
  (begin
    (define total-tests 0)
    (define failed-tests 0)

    (define (get-test-results)
      (list total-tests failed-tests))

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
             (if success "ok" "not ok") " "))
           (display total-tests)
           (display " - ")
           (display (quote code))

           (if (not success)
             (begin
               (display " != ")
               (display expected)))

           (display " ==> ")
           (display actual-result)
           (newline)))))

    (define-syntax perform-test-expect-failure
      (syntax-rules ()
        ((_ eq-op code expected message)
         (let*
           ((actual-result code)
            (success (eq-op actual-result expected)))
           (set! total-tests (+ total-tests 1))
           (set! failed-tests (+ failed-tests
                                 (if success 0 1)))
           (display (string-append
             (if success "ok" "not ok") " "))
           (display total-tests)
           (display " - ")
           (display (quote code))

           (if (not success)
             (begin
               (display " != ")
               (display expected)))

           (display " ==> ")
           (display actual-result)
           (display " ")
           (display message)
           (newline)))))

    (define-syntax testq
      (syntax-rules ()
        ((_ code expected)
         (perform-test eq? code expected))))

    (define-syntax xfailq
      (syntax-rules ()
        ((_ code expected message)
         (perform-test-expect-failure
            eq? code expected (string-append "# TODO: " message)))))

    (define-syntax testv
      (syntax-rules ()
        ((_ code expected)
         (perform-test eqv? code expected))))

    (define-syntax test
      (syntax-rules ()
        ((_ code expected)
         (perform-test equal? code expected))))

    (define (tap-results)
       (display (string-append
                  "1.."
                  (number->string (car (get-test-results)))
                  "\n")))

    (define (result)
      (list (list 'total total-tests)
            (list 'good (- total-tests failed-tests))
            (list 'fail failed-tests)))))
