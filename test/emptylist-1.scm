(import (scheme base)
        (scheme write))

(display "Expect (() . 1): ")
(display (cons '() 1))
(newline)

(display "Expect (() () ()): ")
(display (list '() '() '()))
(newline)

(display "Expect (() ()): ")
(display (list '() '()))
(newline)

(display "Expect (()): ")
(display (list '()))
(newline)

(display "Expect (1): ")
(display (cons 1 '()))
(newline)

(display "Expect (2 1): ")
(display (cons 2 (cons 1 '())))
(newline)
