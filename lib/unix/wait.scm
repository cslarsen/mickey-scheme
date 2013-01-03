#|

   UNIX wait(2) for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (unix wait)
  (export
    coredump?
    exited?
    exitstatus
    signaled?
    stopped?
    stopsig
    termsig
    wait)

  (import
    (mickey library)
    (only (scheme base) define)
    (unix signal))

  (begin
    (open-internal-library "libunix-wait.so")

    ;; Calls wait(2) and returns (<pid> <status>)
    ;;
    ;; For more info, `man 2 wait`
    ;;
    (define wait (bind-procedure "proc_wait"))

    ;; (exited? <status>) returns #t if the process terminated normally by a
    ;; call to _exit(2) or exit(3).
    ;;
    (define exited? (bind-procedure "proc_exitedp"))

    ;; (signaled? <status>) returns #t if the process terminated due to
    ;; receipt of a signal.
    ;;
    (define signaled? (bind-procedure "proc_signaledp"))

    ;; (stopped? <status>) returns #t if the process has not terminated, but
    ;; has stopped and can be restarted.
    ;;
    ;; This procedure can be #t only if the wait call specified the
    ;; WUNTRACED option or if the child process is begin traced (see
    ;; ptrace(2)).
    ;;
    (define stopped? (bind-procedure "proc_stoppedp"))

    ;; If exited? is true, (exitstatus <status>) evaluates to the
    ;; low-order 8 bits of the argument passed to _exit(2) or exit(3) by the
    ;; child.
    ;;
    ;; (Taken from `man 2 wait`)
    ;;
    (define exitstatus (bind-procedure "proc_exitstatus"))

    ;; If signaled? is true, (termsig <status>) evaluates to the
    ;; symbolic signal that caused the termination of the process.
    ;;
    (define (termsig status)
      (integer->signal (proc-termsig status)))

    (define proc-termsig (bind-procedure "proc_termsig"))


    ;; If signaled? is true, (coredump? <status>) evaluates as #t if the
    ;; termination of the process was accompanied by the creation of a core
    ;; file containing an image of the process when the signal was received.
    ;;
    (define coredump? (bind-procedure "proc_coredumpp"))

    ;; If stopped? is true, (stopsig <status>) evaluates to the signal
    ;; (symbol) that caused the process to stop.
    ;;
    (define (stopsig status)
      (integer->signal (proc-stopsig status)))

    (define proc-stopsig (bind-procedure "proc_stopsig"))))
