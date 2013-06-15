(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme repl)
        (portable print)
        (examples lisp-quotes)
        (scheme inquiry)
        (scheme process-context)
        (posix sysconf)
        (posix signal))

(println (implementation-name) " "
         (implementation-version))
(println "Copyright (C) 2013 Christian Stigen Larsen")
(println)
(println (random-quote))
(println)

(print "Running on " (os-type) " " (cpu-architecture))
(if (and (sysconf '_SC_NPROCESSORS_CONF)
         (sysconf '_SC_NPROCESSORS_ONLN))
  (begin
    (println " --- "
             (sysconf '_SC_NPROCESSORS_ONLN)
             "/"
             (sysconf '_SC_NPROCESSORS_CONF)
             " CPUs online")))
(println)

(define (exit-repl s)
  (println "\nExiting ...")
  (exit))

;; Install some signal handlers
(signal 'sigint exit-repl)  ; CTRL+C
(signal 'sigquit exit-repl) ; CTRL+D
(signal 'sigterm exit-repl) ; for graceul exit

;; REPL's environment
(define repl-env (interaction-environment))

;; REPL
(let loop ()
  (print ";# scheme> ")
  (println (eval (read) repl-env))
  (loop))
