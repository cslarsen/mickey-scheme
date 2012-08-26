;; Test out the first simple version of syntax-rules
(import (scheme base))
(import (scheme write))

(display "(1/4) Defining my-when macro\n")

(define-syntax my-when
  (syntax-rules ()
    ((my-when test expr ...)
      (if test (begin expr ...)))))

(display "(2/4) Trying our my-when macro\n")
(my-when #t (display "(3/4) This is gonna be SOO cool!\n"))
(display "(4/4) END of program\n")
