#|
 | Celsius-Fahrenheit temperature table
 |
 | Taken from
 | http://programmingpraxis.com/2012/09/07/the-first-two-programs/2/
 |
 | Ported to current R7RS scheme by Christian Stigen Larsen
 | Public Domain, 2012
 |
 | Works in Mickey Scheme
 |
 | Since R7RS is under development, you have to make a few modifications to
 | make it run under R7RS draft 6, i.e., in Chibi Scheme:
 |
 | - Import (scheme inexact) instead of (scheme math)
 | - Call inexact->exact instead of exact
 |#

(import (scheme write)
        (scheme base)
        (scheme math))

(define (temp-table)
  (do ((f 0 (+ f 20))) ((< 300 f))
    (display f)
    (display #\tab)
    (display (exact (round (* (- f 32) 5/9))))
    (newline)))

(temp-table)
