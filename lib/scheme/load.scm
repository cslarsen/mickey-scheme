#|
Mickey Scheme R7RS character library

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme load)
  (import (only (scheme base) define)
          (mickey library))

  (export load)

  (begin
    (open-internal-library "libscheme-load.so" 'lazy)

    (define load
      (bind-procedure "proc_load"))))
