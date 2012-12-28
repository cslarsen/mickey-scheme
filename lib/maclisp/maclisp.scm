#|

Implementation of *some* MacLisp functions.

They don't behave exactly the same was as in the original MacLisp.

Copyright (C) 2012 Christian Stigen Larsen
Distributed under the GNU LGPL 2.1; see LICENSE.

|#
(define-library (maclisp)
  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (portable atom)
          (portable flatten))

  ;; Missing:
  ;;   PRINT and friends
  ;;   More core forms
  ;;   Reader macros...
  ;;
  (export
    *
    +
    -
    /
    =
    and
    append
    aset'
    atom
    base
    caar
    cadar
    cadddr
    caddr
    cadr
    car
    cddr
    cdr
    cond
    cons
    define
    do
    eq
    explode
    exploden
    funcall
    get
    if
    implode
    labels
    lambda
    list
    memq
    nil
    not
    null
    numberp
    or
    progn
    putprop
    quasiquote
    quote
    reverse
    set
    setq
    sprinter
    t)
  (begin
    ;; TODO: use this variable with (exploden) below
    (define base 8)

    (define atom atom?)

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
          (flatten
            (map string->list (map atom->string atoms))))))

    (define sprinter display)

    (define-syntax funcall
      (syntax-rules ()
        ((funcall f ...)
         (let ((proc (string->symbol (cddr (symbol->string f)))))
           (apply proc ...)))))

    ;; EQ is the same as eq?
    (define eq eq?)

    ;; TODO: the reader form "(<expr>) is the same as (quasiquote <expr>)
    ;; but requires changes to the reader, which is not available in mickey

    ;; same as number?
    (define numberp number?)

    ;; we just define it as #f, but it could be defined as 'NIL or '()
    (define nil #f)

    ;; used for else clause in cond, this is NOT
    ;; the correct way to do it -- the correcty way would
    ;; be to reimplemend cond as a macro, allowing (cond (...) (t <clause>))
    (define t #t)

    ;; See CLTL, 2nd ed, page 240, or
    ;; http://www.maclisp.info/pitmanual/symbol.html#10.6.3
    (define (get sym indicator)
      (let ((found (member indicator sym)))
        (if found (cadr found) nil)))

    (define (null q)
      (if (eq? q nil) t nil))

    ;; (labels ...) is the same as (letrec ...)
    (define-syntax labels
      (syntax-rules ()
        ((_ ...) (letrec ...))))

    ;; same as (begin ...)
    (define-syntax progn
      (syntax-rules ()
        ((_ ...) (begin ...))))

    (define (set sym val)
      (set! sym val))

    (define-syntax setq
      (syntax-rules ()
        ((setq sym val) (set! sym val))))

    ;; http://maclisp.info/pitmanual/symbol.html#10.6.2
    (define-syntax putprop
      (syntax-rules ()
        ((putprop sym val indicator)
         (set! sym (append sym (list indicator val))))
         val))))
