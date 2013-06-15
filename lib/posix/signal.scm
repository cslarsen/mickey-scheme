#|

   POSIX signal facilities for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (posix signal)
  (import (scheme base)
          (mickey library))

  (export
    deactivate-signal
    integer->signal
    signal
    signal->integer
    signal-table)

  (begin
    (open-internal-library "libposix-signal.so" 'lazy 'global)

    (define signal-table
      '( 1 sighup
         2 sigint
         3 sigquit
         4 sigill
         5 sigtrap
         6 sigabrt
         7 sigpoll
         8 sigfpe
         9 sigkill
        10 sigbus
        11 sigsegv
        12 sigsys
        13 sigpipe
        14 sigalrm
        15 sigterm
        16 sigurg
        17 sigstop
        18 sigtstp
        19 sigcont
        20 sigchld
        21 sigttin
        22 sigttou
        23 sigio
        24 sigxcpu
        25 sigxfsz
        26 sigvtalrm
        27 sigprof
        28 sigwinch
        29 siginfo
        30 sigusr1
        31 sigusr2))

    (define (integer->signal number)
      (let ((s (memv number signal-table)))
        (if (pair? s) (cadr s) 'unknown-signal)))

    (define (signal->integer signal)
      (let ((n (memv signal (reverse signal-table))))
        (if (pair? n) (cadr n) 'unknown-signal)))

    (define (parse-signal signal)
      (cond
        ((symbol? signal) (signal->integer signal))
        ((integer? signal)
         (if (member? signal signal-table) signal
             (error (string-append "Unknown signal "
                                   (number->string signal)))))
        (else
          (error "Unknown signal"))))

    (define proc-signal (bind-procedure "proc_signal"))

    ;; Usage: (signal <signal> <procedure taking one argument>)
    ;;
    ;; Registers closure as a handler for the given signal.
    ;;
    ;; Remember that you cannot trap SIGKILL.
    ;;
    (define (signal sig proc)
      (proc-signal (parse-signal sig) proc))

    (define proc-deactivate-signal (bind-procedure "proc_deactivate_signal"))

    ;; Usage: (deactivate-signal <signal>)
    ;;
    ;; Stops handling the given signal.
    ;;
    (define (deactivate-signal sig)
      (proc-deactivate-signal (parse-signal sig)))))
