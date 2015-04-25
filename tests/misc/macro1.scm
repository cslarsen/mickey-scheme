(import (scheme base))
(import (scheme write))

(define-syntax my-when
  (syntax-rules ()
    ((my-when test expr ...)
      (if test (begin expr ...)))))

(define-syntax my-unless
  (syntax-rules ()
    ((my-unless test expr ...)
      (if (not test) (begin expr ...)))))

(my-when #t (display "(1/2) When: Works fine!\n"))
(my-when #f (display "When: Does NOT work\n"))

(my-unless #t (display "Unless: Does NOT work\n"))
(my-unless #f (display "(2/2) Unless: Works fine!\n"));
