(import (scheme base))
(import (scheme write))

(define eat-all-params
  (lambda all-params
    (display "Here is a list: ")
    (display all-params)
    (newline)))

(eat-all-params "one")
(eat-all-params "one" "two")
(eat-all-params "one" "two" "three")
