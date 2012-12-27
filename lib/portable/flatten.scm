#|

Flatten list, taken from
http://rosettacode.org/wiki/Flatten_a_list#Scheme

|#
(define-library (portable flatten)
  (import (scheme base))
  (export flatten)
  (begin
    (define (flatten x)
      (cond ((null? x) '())
            ((not (pair? x)) (list x))
            (else (append (flatten (car x))
                          (flatten (cdr x))))))))

