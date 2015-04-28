(import (scheme base))
(import (scheme write))

(define (circle-area r)
  (let ((pi 3.14159265358979)
        (r^2 (* r r)))
       (* pi r^2)))

(define (calc-area r)
  (display (string-append
    "A circle with radius " (number->string r)
    " has area " (number->string (circle-area r)) "\n")))

(calc-area 0)
(calc-area 1)
(calc-area 2)
(calc-area 3)
(calc-area 4)
(calc-area 5)
