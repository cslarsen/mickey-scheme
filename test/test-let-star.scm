(import (scheme base))
(import (scheme write))

(let* ((a 1)
       (b (+ a 1)))
      (display "Result:\n")
      (display (string-append
        "a = " (number->string a) "\n"
        "b = " (number->string b) "\n"))
      (display "Done!\n"))
