#|

   POSIX sysconf(3) for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see the LICENSE file

|#
(define-library (posix sysconf)
  (export
    sysconf)

  (import
    (mickey library)
    (only (scheme base) define)
    (posix signal))

  (begin
    (open-internal-library "libposix-sysconf.so" 'lazy 'global)
    (define sysconf (bind-procedure "proc_sysconf"))))
