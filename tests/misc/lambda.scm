(import (scheme base))
(import (scheme write))

; simple definition
(display "Let's define `square`\n")

(define square
  (lambda (x)
    (* x x)))

; print some squares
(display "Let's use it!\n")

(display
  (string-append
    "12*12 = " (number->string (square 12)) "\n"
    " 6*6  =  " (number->string (square 6)) "\n"
    " 3*3  =  " (number->string (square 3)) "\n"))

(display "Good bye!\n")
