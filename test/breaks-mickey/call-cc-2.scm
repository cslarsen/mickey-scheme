(define call/cc call-with-current-continuation)
(define cont #f)

(display
  (string-append
    "\n"
    "one: " (number->string 1) "\n"
    "two: " (number->string 2) "\n"
    "three: " (number->string (call/cc
                (lambda (c)
                  (set! cont c) 3))) "\n"
    "four: " (number->string 4) "\n"
    "\n"))

(cont 10)
(cont 20)
