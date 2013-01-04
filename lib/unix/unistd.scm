#|

   UNIX unistd.h for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (unix wait)
  (export
    getpid
    getppid)

  (import
    (mickey library)
    (scheme base))

  (begin
    (open-internal-library "libunix-unistd.so")

    (define getpid (bind-procedure "proc_getpid"))
    (define getppid (bind-procedure "proc_getppid"))))
