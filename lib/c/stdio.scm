#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#

(define-library (c stdio)
  (export
    fclose
    feof
    fflush
    fopen
    fread
    freopen)

  (import (only (scheme base) define)
          (mickey library))

  (begin
    (open-internal-library "libc-stdio.so" 'lazy 'global)

    (define fclose (bind-procedure "proc_fclose"))
    (define feof (bind-procedure "proc_feof"))
    (define fflush (bind-procedure "proc_fflush"))
    (define fopen (bind-procedure "proc_fopen"))
    (define fread (bind-procedure "proc_fread"))
    (define freopen (bind-procedure "proc_freopen"))))
