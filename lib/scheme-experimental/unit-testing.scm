#|
   Experimental unit-testing library for Scheme

   Put in the public domain by the author, Christian Stigen Larsen
|#
(define-library (scheme-experimental unit-testing)
  (import (scheme base)
          (scheme write))
  (export unit-tests assert assert-eqv)
  (begin

    ;; TODO
    ;; Add counters for number of good and failed tests in a unit-tests
    ;; block.
    ;;
    (define-syntax unit-tests
      (syntax-rules ()
        ((_ test ...) test ...)))

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
                   (display "Assertion failed: ")
                   (display code)
                   (newline))))))))

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
                 (display "Assert-eqv failed: ")
                 (display quote-a)
                 (display " != ")
                 (display quote-b)
                 (newline)
                 (display "  Evaluated to: ")
                 (display eval-a)
                 (display " !==> ")
                 (display eval-b)
                 (newline))))))))))
