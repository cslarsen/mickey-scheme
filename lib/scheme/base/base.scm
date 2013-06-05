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
    vector-length
    vector-ref
    vector-set!
    vector?
    when
    write
    xor
    zero?)

  (include "bind-shared-library.scm")
  (include "define-record-type.scm")

  (begin
    (define (compare-all op v)
      (let loop ((v v))
        (if (null? (cadr v)) #t
            (if (or (nan? (car v))
                    (not (op (car v) (cadr v)))) #f
                (loop (cdr v))))))

    (define (>= x1 x2 . xn)
      (compare-all (lambda (a b)
                     (or (> a b) (= a b)))
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

    #|
       R7RS string-map

       Explicitly require at least one string.
    |#
    (define (string-map proc first-string . remaining-strings)
      (let*
        ((output  '())
         (input   (cons first-string remaining-strings))
         (args    (length input))
         (strings (map string->list input)))
        (let loop
          ((chars (map car strings))
           (rest  (map cdr strings)))
          (if (= args (length chars))
            (begin
              (set! output (cons (apply proc chars) output))
              (if (> (apply * (map length rest)) 0)
                (loop 
                  (map car rest)
                  (map cdr rest))))))
        (list->string (reverse output))))

    #|
       R7RS string-for-each

       If n strings are given as input, then `proc` must take n parameters.

       Explicitly requires at least one string.
    |#
    (define (string-for-each proc first-string . remaining-strings)
      (let*
        ((input   (cons first-string remaining-strings))
         (args    (length input))
         (strings (map string->list input)))
        (let loop
          ((chars (map car strings))
           (rest  (map cdr strings)))
          (if (= args (length chars))
            (begin
              (if (> (apply * (map length rest)) 0)
                (begin
                  (apply proc chars)
                  (loop 
                    (map car rest)
                    (map cdr rest)))))))))

    #|
       This is a naive, straight-forward implementation.

       TODO: for-each is supposed to handle circular lists, as long as not
       all of them are circular.

       This implementation will NOT handle that situation, as it will go
       into an infinite loop, instead of raising an error.

       I think it basically means that we have to check for circularity on
       each input.
     |#
    (define (for-each procedure list1 . etc)
      (let*
        ((lists (cons list1 etc))
         (count (length lists)))
        (let loop
          ((arguments (map car lists))
           (remaining (map cdr lists)))
          ;;
          ;; terminate when the shortest list is finished
          (if (= (length arguments) count)
            (begin
              ;; call procedure with input parameters
              (apply procedure arguments)

              ;; ... and keep going
              (loop (map car remaining)
                    (map cdr remaining)))))))

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
