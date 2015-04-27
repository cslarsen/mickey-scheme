#|

   POSIX unistd.h for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (posix wait)
  (export
    gethostname
    getpid
    getppid
    usleep)

  (import
    (mickey library)
    (scheme base))

  (begin
    (open-internal-library-determine-extension "libposix-unistd" 'lazy 'global)

    (define gethostname (bind-procedure "proc_gethostname"))
    (define getpid (bind-procedure "proc_getpid"))
    (define getppid (bind-procedure "proc_getppid"))
    (define usleep (bind-procedure "proc_usleep"))))
