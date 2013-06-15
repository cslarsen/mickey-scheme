#|

   POSIX kill(2) for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (posix wait)
  (export kill)

  (import
    (mickey library)
    (scheme base)
    (posix signal))

  (begin
    (open-internal-library "libposix-kill.so" 'global 'lazy)

    (define proc-kill (bind-procedure "proc_kill"))

    ;; <signal> can be either a symbol or integer number for a signal.
    ;;
    ;; Examples:
    ;;
    ;;   (kill 1234 'sighup)
    ;;   (kill 1234 1)
    ;;
    (define (kill pid signal)
      (when (not (integer? pid)) (error "(kill) requires an integer PID"))
      (proc-kill pid (cond
                       ((symbol? signal) (signal->integer signal))
                       ((integer? signal) signal)
                       (else
                         (error "(kill) requires <signal> to be an integer or symbol")))))))
