#|

Mickey R7RS Scheme

Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme eval)
  (import (scheme base)
          (mickey library))
  (export eval)
  (begin
    (define eval 'dummy-value)))
