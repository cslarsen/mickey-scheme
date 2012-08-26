(import (scheme base))
(import (scheme write))

(display "Should display 'One argument': ")
(apply display (list (list "One argument"))) 
(newline)

(display "Should display '6': ")
(display (apply + (list 1 2 3)))
(newline)
