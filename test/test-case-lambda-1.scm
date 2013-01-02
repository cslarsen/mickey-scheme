(import (scheme base)
        (scheme write)
        (scheme case-lambda))

(define foo
  (case-lambda
    ((a) "one arg")
    ((a b) "two args")
    ((a . r) "at least 1 arg")
    ((a b . r) "at least 2 args")
    (r "any number of args")))

(define test
  (lambda args
    (display "(foo ")
    (display args)
    (display ") ==> ")
    (display (apply foo args))
    (newline)))

(test 1)
(test 1 2)
(test 1 2 3)
(test 1 2 3 4)
(test 1 2 3 4 5 6 7 8 9)
