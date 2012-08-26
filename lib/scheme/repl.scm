#|

Mickey R7RS Scheme

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme repl)
  (import (only (scheme base) define)
          (mickey library))

  (export interaction-environment)

  (begin
    (open-internal-library "libscheme-repl.so" 'lazy)

    (define interaction-environment
      (bind-procedure "proc_interaction_environment"))))
