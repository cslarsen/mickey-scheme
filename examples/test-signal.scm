;; Demonstration of trapping UNIX signals in Mickey R7RS Scheme
;; https://github.com/cslarsen/mickey-scheme
;;
;; Put in the public domain by the author,
;; Christian Stigen Larsen
;;
;; Example run:
;;
;; $ mickey examples/test-signal.scm
;; Installing signal handlers
;;
;; In another terminal window, try sending signals
;; to this program (pid 19240), for example:
;;
;;   $ kill -s SIGHUP 19240
;;   $ kill -s SIGINT 19240
;;
;; Waiting for signal .......
;; Caught signal sighup (1)
;; ......^C
;; Caught signal sigint (2)
;; We stop looping when we catch SIGINT
;; Exiting gracefully ...
;;
(import (unix signal)
        (unix unistd)
        (c stdio)
        (portable print)
        (scheme base))

(define *keep-looping* #t)

(define (catch-signal sig)
  (let ((signal (integer->signal sig)))
    (println "\nCaught signal " signal " (" sig ")")
    (fflush (stdout))

    ;; Terminate if we catch SIGINT
    (if (eqv? signal 'sigint)
      (begin
        (println "We stop looping when we catch SIGINT")
        (fflush (stdout))
        (set! *keep-looping* #f)))))

(printsln "Installing signal handlers")

(signal 'sighup catch-signal)
(signal 'sigint catch-signal)
(signal 'sigterm catch-signal)

(println "\nIn another terminal window, try sending signals\n"
         "to this program (pid " (getpid) "), for example:\n"
         "\n"
         "  $ kill -s SIGHUP " (getpid) "\n"
         "  $ kill -s SIGINT " (getpid) "\n")

(print "Waiting for signal ..")

;; Loop until we catch SIGINT
(let loop ()
  (print ".")
  (fflush (stdout))
  (usleep 1000000)
  (if *keep-looping* (loop)))

(println "Exiting gracefully ...")
