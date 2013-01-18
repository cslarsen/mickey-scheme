(import (portable print)
        (scheme base)
        (scheme write)
        (experimental eval-with-continuation))

(println "Starting up")

(define program '(+ 1 2 (* 5 6)))
(println "program: " program)
(println "eval-cont: <" (eval-with-continuation program) ">")
