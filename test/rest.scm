(import (scheme base))
(import (scheme write))

(define (print-each prefix . rest)
  (define (print-one x)
    (display (string-append prefix
      (if (string? x) x (number->string x))
      "\n")))
  (define (print-list xs)
    (if (not (null? xs))
        (begin
          (print-one (car xs))
          (print-list (cdr xs)))
        0))
  (print-list rest))

(print-each "bottles on the wall: "
  100 99
  "*take one down, pass it around*"
  "*heavy drinking*"
  2 1 0)
