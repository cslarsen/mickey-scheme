#|

   POSIX exit(3) for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (posix fork)
  (export exit)

  (import (only (scheme base) define)
          (mickey library))
  (begin
    (open-internal-library "libposix-exit.so" 'lazy 'global)

    ;; Calls exit(3) with given integer status (exit code).
    ;;
    ;; For more info, `man 3 exit`
    ;;
    (define exit
      (bind-procedure "proc_exit"))))
