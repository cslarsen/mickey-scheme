#|

Library: (mickey misc)

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(define-library (mickey internals)
  (import (only (scheme base)
                define)
          (only (mickey library)
                open-internal-library bind-procedure))
  (export
    global-options)

  (begin
    (open-internal-library "libmickey-internals.so")

    (define global-options
      (bind-procedure "global_options"))))
