(import (scheme base))
(import (scheme write))

(display (string-append
  "Expecting 6: "
  (number->string
    (+ 1 . (2 . (3 . ()))))
  "\n"))
