(import (scheme base))
(import (scheme write))

(display "Numbers from 1 to 10: ")
(do ((i 1 (+ i 1))
     (x 1 (+ x 2)))
    ((> i 10) (newline))
      (display i)
      (display " "))
