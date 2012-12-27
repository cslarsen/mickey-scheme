#|

Implementation of some MacLisp functions.

Copyright (C) 2012 Christian Stigen Larsen
Distributed under the GNU LGPL 2.1; see LICENSE.

|#
(define-library (maclisp)
  (import (scheme base))
  (export
    explode
    exploden
    implode)
  (begin
    (define base 8)

    #|

    Transforms symbol to list of characters.

    Example:
    (explode 'hey) ==> (#\h #\e #\y)

    This function is not completely compatible with MacLisp,
    see http://www.maclisp.info/pitmanual/charac.html#11.3.1

    |#
    (define (explode symbol)
      (map string->symbol
        (map list->string
          (map list
            (string->list
              (symbol->string symbol))))))

    ;; As (explode) but returns numbers.
    ;;
    (define (exploden symbol)
      (map char->integer (string->list (symbol->string symbol))))

    #|

    Concatenates list of symbols to one symbol.

    Examples:
    (implode '(h e y)) ) ==> hey
    (implode (cdr (explode 'hello))) ==> 'ello

    |#
    (define (implode symbols)
      (string->symbol
        (list->string
          (map car
            (map string->list (map symbol->string symbols))))))
