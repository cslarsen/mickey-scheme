#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#

(define-library (unix uname)
  (export uname)

  (import (only (scheme base) define)
          (mickey library))

  (begin
    (open-internal-library "libunix-uname.so" 'lazy)

    (define uname
      (bind-procedure "proc_uname"))))
