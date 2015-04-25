(import (scheme base))
(import (scheme write))

;; TestR7RS support for (case)

#|
 | Output should be:
 |
 | Expecting composite        : composite
 | Expecting unspecified value: 
 | Expecting (c c c)          : (c c c)
 |
 |#
(display "Expecting composite        : ")
(display
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite))) ; ==> composite
(newline)

(display "Expecting unspecified value: ")
(display
  (case (car '(c d))
    ((a) 'a)
    ((b) 'b))) ; ==> unspecified
(newline)

(display "Expecting (c c c)          : ")
(display
  (case (car '(c d))
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else => (lambda (x) (list x x x))))) ; ==> (c c c)
(newline)
