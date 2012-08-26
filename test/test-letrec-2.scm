(import (scheme base))
(import (scheme write))

(display "Start\n")

(letrec ((val1 123)
         (val2 456))
        (display val1) (newline)
        (display val2) (newline))

(display "Outside\n")

; These will and SHOULD give errors
; (display val1) (newline)
; (display val2) (newline))
