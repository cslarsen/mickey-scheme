#|

   POSIX sysconf(3) for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see the LICENSE file

   Example:

   > (import (posix sysconf))
   > (sysconf (car (sysconf-symbols)))

|#
(define-library (posix sysconf)
  (export
    sysconf
    sysconf-symbols)

  (import
    (mickey library)
    (scheme base)
    (posix signal))

  (begin
    (open-internal-library-determine-extension "libposix-sysconf" 'lazy 'global)

    ;; Syntax: (sysconf <symbol>)
    ;;
    ;; Returns value of given sysconf setting.
    ;;
    (define sysconf (bind-procedure "proc_sysconf"))

    ;; Syntax: (sysconf-symbols)
    ;;
    ;; Returns list of found sysconf variables on this system.
    ;;
    (define sysconf-symbols (bind-procedure "proc_sysconf_symbols"))))
