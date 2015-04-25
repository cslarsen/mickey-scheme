(import (scheme base))
(import (scheme write))

; named let
; should produce the output "10 9 8 7 6 5 4 3 2 1\n"
(let foo
  ((count 10))
  (display count)
  (display " ")
  (if (> count 1)
    (foo (- count 1))
    (display "\n")))
