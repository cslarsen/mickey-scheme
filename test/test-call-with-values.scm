(import (scheme base)
        (scheme write))

(define (foo)
  (values 1 2))

(define (bar x y)
  (display "bar called with parameters:") (newline)
  (display "  x=") (display x) (newline)
  (display "  y=") (display y) (newline))

(call-with-values foo bar)
