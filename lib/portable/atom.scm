(define-library (portable atom)
  (import (scheme base))
  (export atom?)
  (begin
    (define (atom? x)
      (and (not (pair? x)) (not (null? x))))))
