#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#

(define-library (posix uname)
  (export uname)

  (import (only (scheme base) define)
          (mickey library))

  (begin
    (open-internal-library-determine-extension "libposix-uname" 'lazy 'global)

    (define uname
      (bind-procedure "proc_uname"))))
