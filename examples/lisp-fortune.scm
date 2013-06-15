;; Prints a random Lisp/Scheme quote

(import (portable print)
        (examples lisp-quotes))

(println (random-quote))
