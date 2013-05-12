#|

   Part of Mickey R7RS Scheme

   Copyright (C) 2011-2013 Christian Stigen Larsen

   Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
   See the file LICENSE for full text.

|#
(define-library (portable atom?)
  (import (scheme base))
  (export
    atom->string
    atom?)
  (begin
    ;; Atom predicate taken from The Little Schemer
    (define (atom? x)
      (and (not (pair? x)) (not (null? x))))

    ;; Convert (almost) any atom to string
    (define (atom->string x)
      (cond
        ((string?  x) x)
        ((number?  x) (number->string x))
        ((char?    x) (list->string (list x)))
        ((boolean? x) (boolean->string x))
        ((vector?  x) (vector->string x))
        ((symbol?  x) (symbol->string x))
        (else
          (error "Unsupported atom in atom->string"))))))
