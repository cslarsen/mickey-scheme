(import (scheme base)
        (scheme write)
        (experimental eval-with-continuation)
        (portable print))

(define program (quote
  (display (string-append
             "RESULT: <"
             (number->string (+ 1 2 (* 4 5)))
             ">\n"))))

(display (eval-with-continuation program))
