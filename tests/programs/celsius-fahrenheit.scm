#|
 | Celsius-Fahrenheit temperature table
 |
 | Taken from
 | http://programmingpraxis.com/2012/09/07/the-first-two-programs/2/
 |
 | Ported to current R7RS scheme by Christian Stigen Larsen
 |#

(import (scheme write)
        (scheme base)
        (scheme inexact)
        (test unit-test))

(define (fahrenheit->celsius degrees)
  (exact (round (* (- degrees 32) 5/9))))

(define (temp-table)
  (display "Fahrenheit\tCelsius")
  (newline)
  (do ((f 0 (+ f 20))) ((< 300 f))
    (display f)
    (display #\tab)
    (display #\tab)
    (display (exact (round (* (- f 32) 5/9))))
    (newline)))

(let
  ((name "celsius-fahrenheit.scm"))
  (testq name (fahrenheit->celsius   0) -18)
  (testq name (fahrenheit->celsius  20) -7)
  (testq name (fahrenheit->celsius  40) 4)
  (testq name (fahrenheit->celsius  60) 16)
  (testq name (fahrenheit->celsius  80) 27)
  (testq name (fahrenheit->celsius 100) 38)
  (testq name (fahrenheit->celsius 120) 49)
  (testq name (fahrenheit->celsius 140) 60)
  (testq name (fahrenheit->celsius 160) 71)
  (testq name (fahrenheit->celsius 180) 82)
  (testq name (fahrenheit->celsius 200) 93)
  (testq name (fahrenheit->celsius 220) 104)
  (testq name (fahrenheit->celsius 240) 116)
  (testq name (fahrenheit->celsius 260) 127)
  (testq name (fahrenheit->celsius 280) 138)
  (testq name (fahrenheit->celsius 300) 149))
