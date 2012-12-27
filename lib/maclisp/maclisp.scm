#|

Implementation of some MacLisp functions.

Copyright (C) 2012 Christian Stigen Larsen
Distributed under the GNU LGPL 2.1; see LICENSE.

|#
(define-library (maclisp)
  (import (scheme base))
  (export
    explode
    implode)
  (begin
    #|

    Transforms symbol to list of characters.

    Example:
    (explode 'hey) ==> (#\h #\e #\y)

    |#
    (define (explode symbol)
      (map string->symbol
        (map list->string
          (map list
            (string->list
              (symbol->string symbol))))))

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
