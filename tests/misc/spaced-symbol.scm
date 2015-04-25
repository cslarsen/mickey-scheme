(import (scheme base)
        (scheme write))

(define |foo bar| 123)
(display "|foo bar|: ")
(display |foo bar|)
(newline)

(define baz '|foo bar baz|)
(display "baz: ")
(display baz)
(newline)

(define (|squared number| x)
  (* x x))

(display "12^2 is ")
(display (|squared number| 12))
(newline)
