#|

Mickey R7RS Scheme

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

The larger part of this library resides in libscheme-base.so.

|#

(define-library (scheme base)
  (import (mickey library)
          (srfi 0)) ;; cond-expand

  (export
    *
    +
    -
    /
    <
    <=
    =
    >
    >=
    abs
    and
    append
    apply
    assoc
    assq
    assv
    begin
    binary-port?
    boolean->string
    boolean?
    bytevector
    bytevector-copy
    bytevector-copy!
    bytevector-copy-partial
    bytevector-copy-partial!
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector?
    caar
    cadr
    car
    case
    cdar
    cddr
    cdr
    char->integer
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    close-port
    cond
    cond-expand
    cons
    current-error-port
    current-input-port
    current-output-port
    define
    define-record-type
    define-syntax
    do
    eof-object
    eof-object?
    eq?
    equal?
    eqv?
    error
    even?
    exact
    exact->inexact
    exact-integer?
    exact?
    expt
    features
    finite?
    for-each
    gcd
    if
    inexact
    inexact?
    infinite?
    input-port?
    integer->char
    integer?
    lambda
    lcm
    length
    let
    let*
    letrec
    letrec*
    list
    list->string
    list->vector
    list-ref
    list-set!
    list-tail
    list?
    make-bytevector
    make-list
    make-string
    make-vector
    map
    max
    member
    memq
    memv
    min
    modulo
    nan?
    negative?
    newline
    not
    null?
    number->string
    number?
    odd?
    or
    output-port?
    pair?
    peek-char
    port-open?
    port?
    positive?
    procedure?
    quasiquote
    quote
    rational?
    read-line
    real?
    reverse
    round
    set!
    set-car!
    set-cdr!
    string
    string->list
    string->number
    string->symbol
    string->vector
    string-append
    string-for-each
    string-length
    string-map
    string-ref
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol?
    textual-port?
    truncate
    unless
    values
    call-with-values
    vector
    vector->list
    vector->string
    vector-copy
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    vector?
    when
    write
    xor
    zero?)

  (include "bind-shared-library.scm")
  (include "define-record-type.scm")
  (include "make-list.scm")
  (include "map.scm")
  (include "for-each.scm")
  (include "string-map.scm")
  (include "string-for-each.scm")
  (include "positive?.scm")
  (include "negative?.scm")
  (include "zero?.scm")
  (include "member.scm")
  (include "vector-map.scm")
  (include "vector-for-each.scm")
  (include "assoc.scm")
  (include "number?.scm")
  (include "abs.scm")

  (begin
    (define (compare-all cmp? v)
      (cond
        ((or (null? v)
             (null? (cdr v))) #t)
        ((cmp? (car v)
               (cadr v))
         (compare-all cmp? (cdr v)))
        (else
          #f)))

    (define (>= x1 x2 . xn)
      (compare-all (lambda (a b)
                     (or (> a b)
                         (= a b)))
                   `(,x1 ,x2 ,@xn)))

    (define (<= x1 x2 . xn)
      (compare-all (lambda (a b) (or (< a b) (= a b)))
                   `(,x1 ,x2 ,@xn)))

    (define-syntax when
      (syntax-rules ()
        ((when test expr ...)
         (if test (begin expr ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((unless test expr ...)
         (if (not test) (begin expr ...)))))

    (define (caar s) (car (car s)))
    (define (cadr s) (car (cdr s)))
    (define (cdar s) (cdr (car s)))
    (define (cddr s) (cdr (cdr s)))

    (define (exact-integer? z)
      (and (integer? z) (exact? z)))

    ;; Code taken from R7RS draft
;    (define (values . things)
;      (call-with-current-continuation
;        (lambda (cont) (apply cont things))))

    ;; TODO: Add (complex?)
    (define (number? n)
      (or (real? n) (integer? n) (rational? n)))

    (define internal-char-foldcase (bind-procedure "proc_base_char_foldcase"))

    (define (char-ci=? char1 char2 . charN)
      (let* ((chars `(,char1 ,char2 ,@charN))
             (folded-chars (map internal-char-foldcase chars)))
        (apply char=? folded-chars)))

    (define (char-ci<? char1 char2 . charN)
      (let* ((chars `(,char1 ,char2 ,@charN))
             (folded-chars (map internal-char-foldcase chars)))
        (apply char<? folded-chars)))

    (define (char-ci>? char1 char2 . charN)
      (let* ((chars `(,char1 ,char2 ,@charN))
             (folded-chars (map internal-char-foldcase chars)))
        (apply char>? folded-chars)))

    (define (char-ci>=? char1 char2 . charN)
      (let ((chars `(,char1 ,char2 ,@charN)))
        (or (char-ci=? chars) (char-ci>? chars))))

    (define (char-ci<=? char1 char2 . charN)
      (let ((chars `(,char1 ,char2 ,@charN)))
        (or (char-ci=? chars) (char-ci<? chars))))

    ;;
    ;; This implementation of values and call-with-values
    ;; is a dirty, ugly hack. :-)
    ;;
    (define-syntax values
      (syntax-rules ()
        ((_ params ...) (list '(values) params ...))))
    ;;
    (define-syntax call-with-values
      (syntax-rules ()
        ((call-with-values producer consumer)
         (let ((c consumer)
               (p producer))
           (if (not (procedure? p))
               (error "call-with-values requires a (values) object"))
           (apply c (cdr (p)))))))

    ;; open libmickey.so
    (open-self 'global 'lazy)
    (define features (bind-procedure "proc_list_features"))))
