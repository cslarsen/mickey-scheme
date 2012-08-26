(import (scheme base))
(import (scheme write))

(define var "goose")

(let ((var 10))
  (display "Value of `var` is ")
  (display var)
  (newline))
