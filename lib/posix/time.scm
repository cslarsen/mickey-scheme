#|

POSIX time(3) and friends for Mickey Scheme

Copyright (C) 2013 Christian Stigen Larsen
Distributed under the LGPL 2.1; see LICENSE

Example usage:

  > (import (posix time))
  > (define now (time))
  > (ctime now)
  "Sun May 12 23:20:10 2013\n"
  > (time->list (localtime now))
  ((seconds 10) (minutes 20) (hours 23) (month-day 12) (month may) (year 2013)
   (week-day sunday) (year-day 131) (dst? #t) (timezone CEST)
   (utc-offset-seconds 7200))
  > (cadr (assv 'year (time->list (localtime now))))
  2013

|#

(define-library (posix time)
  (export
    ctime
    gmtime
    integer->month
    localtime
    month
    month->integer
    months
    time
    time-value
    time->list)

  (import
    (mickey library)
    (only (scheme base) define))

  (begin
    (open-internal-library-determine-extension "libposix-time" 'lazy 'global)

    (define months
      #(january
        february
        march
        april
        may
        june
        july
        august
        september
        october
        november
        december))

    (define (integer->month number)
      "Return month with given number in the range 1 to 12."
      (if (or (< number 1) (> number 12))
          (error "Month number must be in the range 1 to 12.")
          (vector-ref months (- number 1))))

    (define (month->integer month)
      "Return the given month's number, between 1 and 12."
      (let next
        ((number 1))
        (if (eqv? month (integer->month number))
          number
          (if (< number 12)
              (next (+ 1 number))
              (error "Not a recognized month")))))

    ;; Calls time(3) and returns pointer to time_t value.
    ;;
    ;; For more info, `man 3 time`
    ;;
    (define time (bind-procedure "proc_time"))
    (define time-value (bind-procedure "proc_time_value"))
    (define localtime (bind-procedure "proc_localtime"))
    (define gmtime (bind-procedure "proc_gmtime"))
    (define time->list (bind-procedure "proc_tm_to_alist"))
    (define ctime (bind-procedure "proc_ctime"))))
