#|
 | Celsius-Fahrenheit temperature table
 |
 | Taken from
 | http://programmingpraxis.com/2012/09/07/the-first-two-programs/2/
 |
 | Ported to current R7RS scheme by Christian Stigen Larsen
 | Public Domain, 2012-2013
 |
 |#

(import (scheme write)
        (scheme base)
        (scheme inexact))

(define (temp-table)
  (do ((f 0 (+ f 20))) ((< 300 f))
    (display f)
    (display #\tab)
    (display (exact (round (* (- f 32) 5/9))))
    (newline)))

(temp-table)
