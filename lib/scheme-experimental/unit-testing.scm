#|

   Experimental unit-testing library for Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0

|#
(define-library (scheme-experimental unit-testing)
  (import (scheme base)
          (scheme write))
  (export unit-tests assert assert-eqv)
  (begin

    (define *name* "<unknown test name>")
    (define *good* 0)
    (define *bad* 0)

    (define (mark-bad)
      (set! *bad* (+ 1 *bad*)))

    (define (mark-good)
      (set! *good* (+ 1 *good*)))

    (define (set-name x)
      (set! *name* x))

    (define (report)
      (display (string-append "Test results for " *name*))
      (newline)

      (display (string-append
                 (number->string *good*) "/"
                 (number->string (+ *good* *bad*))
                 " tests OK"))
      (newline)

      (display (string-append
                 (number->string *bad*) " failed"))
      (newline))

    ;; TODO
    ;; Add counters for number of good and failed tests in a unit-tests
    ;; block.
    ;;
    (define-syntax unit-tests
      (syntax-rules ()
        ((_ name ...) (begin (set-name name) (begin name ...) (report)))))

    ;; Assert that given result is true
    ;;
    ;; TODO: Allow unlimited parameters, assert that each one is true.
    ;;
    (define-syntax assert
      (syntax-rules ()
        ((_ assertion ...)
         (begin
           (let* ((code (quote assertion ...))
                  (result assertion ...))
             (if (not result)
                 (begin
                   (mark-bad)
                   (display "Assertion failed: ")
                   (display code)
                   (newline))
                 (mark-good)))))))

    (define-syntax assert-eqv
      (syntax-rules ()
        ((_ a b)
         (begin
           (let* ((quote-a (quote a))
                  (quote-b (quote b))
                  (eval-a a)
                  (eval-b b))
             (if (not (eqv? eval-a eval-b))
               (begin
                 (mark-bad)
                 (display "Assert-eqv failed: ")
                 (display quote-a)
                 (display " != ")
                 (display quote-b)
                 (newline)
                 (display "  Evaluated to: ")
                 (display eval-a)
                 (display " !==> ")
                 (display eval-b)
                 (newline))
               (mark-good)))))))))
