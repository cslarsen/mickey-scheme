#|

   UNIX time(3) and friends for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (unix time)
  (export
    ctime
    time
    time-value)

  (import
    (mickey library)
    (only (scheme base) define))

  (begin
    (open-internal-library "libunix-time.so" 'lazy 'global)

    ;; Calls time(3) and returns pointer to time_t value.
    ;;
    ;; For more info, `man 3 time`
    ;;
    (define time (bind-procedure "proc_time"))
    (define time-value (bind-procedure "proc_time_value"))
    (define ctime (bind-procedure "proc_ctime"))))
