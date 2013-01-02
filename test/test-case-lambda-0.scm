(import (scheme base)
        (scheme write)
        (scheme case-lambda))

(define foo
  (case-lambda
    ((first . rest)
       (display "first: ")
       (display first)

       (display " rest: ")
       (display rest)
       (newline))))

(foo 1)
(foo 1 2)
(foo 1 2 3)
(foo 1 2 3 4)
