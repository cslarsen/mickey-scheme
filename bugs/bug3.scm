; FAIL:
(import  (prefix (scheme base) base-) (scheme write))

; OK:
;(import (scheme base) (scheme write))

(define no-print #f)
(display "Hello, ")

; FAIL:
(display (base-unless no-print "world!"))

; OK:
;(display (unless no-print "world!"))

(display "\n")
