#|

Mickey R7RS Scheme

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme write)
  (import (only (scheme base) define)
          (mickey library))

  (export display)

  (begin
    (open-internal-library "libscheme-write.so")

    (define display
      (bind-procedure "proc_display"))))
