(import (scheme base))
(import (scheme write))

; Standard factorial function,
; from Queinnec, p. 54
(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(define (show-fact n)
  (display (string-append
    (number->string n) "! = " (number->string (fact n)) "\n")))

(show-fact 0)
(show-fact 1)
(show-fact 2)
(show-fact 3)
(show-fact 4)
(show-fact 5)
(show-fact 6)
(show-fact 7)
(show-fact 8)
(show-fact 9)
(show-fact 10)
(show-fact 11)
(show-fact 12)
(show-fact 13) ; overflows 32-bit int
(show-fact 20)
(show-fact 40)
(show-fact 80)
(show-fact 100)
