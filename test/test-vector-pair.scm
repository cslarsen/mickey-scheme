(import (scheme base))
(import (scheme write))

(define (foo list-or-vector)
  (display "Got: ")
  (display list-or-vector)
  (display "\n"))

(foo (make-vector 5 0))
(foo (list 0 0 0 0 0))
