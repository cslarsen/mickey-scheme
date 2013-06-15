(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme repl)
        (portable print)
        (examples lisp-quotes)
        (scheme inquiry))

(println (implementation-name) " "
         (implementation-version))
(println "Copyright (C) 2013 Christian Stigen Larsen")
(println)
(println (random-quote))

(define env (interaction-environment))

(let loop
  ()
  (let*
    ((foo (print "> "))
     (in (read)))
    (println (eval in env)))
  (loop))
