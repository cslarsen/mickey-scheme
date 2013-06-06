#|

Provides boolean logic operators.

Copyright (C) 2013 Christian Stigen Larsen

Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
See the file LICENSE for full text.

Provides boolean logic operators such as boolean/and and friends.  These
are procedural instead of syntactic, so they will eagerly evaluate all
their arguments.

The reason for this module is that one cannot do

 (apply and (list #t #t #t))

because and is a macro.  This module lets one instead use

 (apply boolean/and (list #t #t #t))

TODO:

- Add boolean=?
- Add boolean/or

|#
(define-library (portable boolean-logic)
  (import (scheme base))
  (export
    boolean/and)
  (begin
    (define (boolean/and . args)
      (if (null? args)
          #t
          (if (car args)
              (apply boolean/and (cdr args))
              #f)))))
