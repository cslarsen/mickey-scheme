; This is a regression test for Mickey Scheme,
; where the program below produced the WRONG
; answer.

(import (scheme base))
(import (scheme write))

(define <=
  (lambda (x y)
    (or (= x y)
        (< x y))))

(define a -1.0)
(define b  1.0)

(if (<= a b)
    (display "-1.0 <= 1.0 is correct\n")
    (display "-1.0 >  1.0 is NOT correct\n"))
