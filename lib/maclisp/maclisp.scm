#|

Implementation of *some* MacLisp functions.

They don't behave exactly the same was as in the original MacLisp.

Copyright (C) 2012 Christian Stigen Larsen
Distributed under the GNU LGPL 2.1; see LICENSE.

|#
(define-library (maclisp)
  (import (scheme base)
          (portable atom))
  (export
    aset'
    explode
    exploden
    implode)
  (begin
    (define base 8)

    #|

    Apparently same as set!, but returns the new value.

    |#
    (define-syntax aset'
      (syntax-rules ()
        ((_ a b) (begin
                   (set! a b)
                   a))))

    #|

    Transforms symbol to list of characters.

    Example:
    (explode 'hey) ==> (#\h #\e #\y)

    This function is not completely compatible with MacLisp,
    see http://www.maclisp.info/pitmanual/charac.html#11.3.1

    |#
    (define (explode atom)
      (map string->symbol
        (map list->string
          (map list
            (string->list
              (atom->string atom))))))

    #|

    As (explode) but returns numbers.

    |#
    (define (exploden atom)
      (map char->integer (string->list (atom->string atom))))

    #|

    Concatenates list of symbols to one symbol.

    Examples:
    (implode '(h e y)) ) ==> hey
    (implode (cdr (explode 'hello))) ==> 'ello

    |#
    (define (implode atoms)
      (string->symbol
        (list->string
          (map car
            (map string->list (map atom->string atoms))))))
