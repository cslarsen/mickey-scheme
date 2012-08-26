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
  (import (mickey library))
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
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-port
    cond
    cons
    current-error-port
    current-input-port
    current-output-port
    define
    define-syntax
    do
    eq?
    equal?
    eqv?
    error
    even?
    expt
    file-exists?
    finite?
    for-each
    gcd
    if
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
    list
    list->string
    list->vector
    list-ref
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
    port-open?
    port?
    positive?
    procedure?
    quasiquote
    quote
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

  (begin
    (open-internal-library "libscheme-base.so")

    (define * (bind-procedure "proc_mul"))
    (define + (bind-procedure "proc_add"))
    (define - (bind-procedure "proc_sub"))
    (define / (bind-procedure "proc_divf"))
    (define < (bind-procedure "proc_less"))
    (define <= (bind-procedure "proc_lteq"))
    (define = (bind-procedure "proc_eqintp"))
    (define > (bind-procedure "proc_greater"))
    (define >= (bind-procedure "proc_gteq"))
    (define abs (bind-procedure "proc_abs"))
    (define and (bind-procedure "proc_and"))
    (define append (bind-procedure "proc_append"))
    (define apply (bind-syntax "proc_dummy_placeholder"))
    (define assoc (bind-procedure "proc_assoc"))
    (define assq (bind-procedure "proc_assq"))
    (define assv (bind-procedure "proc_assv"))
    (define begin (bind-syntax "proc_begin"))
    (define binary-port?  (bind-procedure "proc_binary_portp"))
    (define boolean->string (bind-procedure "proc_boolean_to_string"))
    (define boolean?  (bind-procedure "proc_booleanp"))
    (define bytevector-copy (bind-procedure "proc_bytevector_copy"))
    (define bytevector-copy!  (bind-procedure "proc_bytevector_copy_bang"))
    (define bytevector-copy-partial (bind-procedure "proc_bytevector_copy_partial"))
    (define bytevector-copy-partial!  (bind-procedure "proc_bytevector_copy_partial_bang"))
    (define bytevector-length (bind-procedure "proc_bytevector_length"))
    (define bytevector-u8-ref (bind-procedure "proc_bytevector_u8_ref"))
    (define bytevector-u8-set!  (bind-procedure "proc_bytevector_u8_set_bang"))
    (define bytevector?  (bind-procedure "proc_bytevectorp"))
    (define car (bind-procedure "proc_car"))
    (define case (bind-procedure "proc_case"))
    (define cdr (bind-procedure "proc_cdr"))
    (define char->integer (bind-procedure "proc_char_to_integer"))
    (define char<=?  (bind-procedure "proc_char_ltep"))
    (define char<?  (bind-procedure "proc_char_ltp"))
    (define char=?  (bind-procedure "proc_char_eqp"))
    (define char>=?  (bind-procedure "proc_char_gtep"))
    (define char>?  (bind-procedure "proc_char_gtp"))
    (define char?  (bind-procedure "proc_charp"))
    (define close-port (bind-procedure "proc_close_port"))
    (define cond (bind-syntax "proc_cond"))
    (define cons (bind-procedure "proc_cons"))
    (define current-error-port (bind-procedure "proc_current_error_port"))
    (define current-input-port (bind-procedure "proc_current_input_port"))
    (define current-output-port (bind-procedure "proc_current_output_port"))
    (define define (bind-syntax "proc_define"))
    (define define-syntax (bind-syntax "proc_define_syntax"))
    (define do (bind-syntax "proc_do"))
    (define eq?  (bind-procedure "proc_eqp"))
    (define equal?  (bind-procedure "proc_equalp"))
    (define eqv?  (bind-procedure "proc_eqvp"))
    (define error (bind-procedure "proc_error"))
    (define even?  (bind-procedure "proc_evenp"))
    (define expt (bind-procedure "proc_expt"))
    (define file-exists?  (bind-procedure "proc_file_existsp"))
    (define finite?  (bind-procedure "proc_finitep"))
    (define gcd (bind-procedure "proc_gcd"))
    (define if (bind-syntax "proc_dummy_placeholder"))
    (define infinite?  (bind-procedure "proc_infinitep"))
    (define input-port?  (bind-procedure "proc_input_portp"))
    (define integer->char (bind-procedure "proc_integer_to_char"))
    (define integer?  (bind-procedure "proc_integerp"))
    (define lambda (bind-syntax "proc_dummy_placeholder"))
    (define lcm (bind-procedure "proc_lcm"))
    (define length (bind-procedure "proc_length"))
    (define let (bind-syntax "proc_let"))
    (define let* (bind-syntax "proc_letstar"))
    (define letrec (bind-syntax "proc_letrec"))
    (define list (bind-procedure "proc_list"))
    (define list->string (bind-procedure "proc_list_to_string"))
    (define list->vector (bind-procedure "proc_list_to_vector"))
    (define list-ref (bind-procedure "proc_list_ref"))
    (define list-tail (bind-procedure "proc_list_tail"))
    (define list?  (bind-procedure "proc_listp"))
    (define make-bytevector (bind-procedure "proc_make_bytevector"))
    (define make-string (bind-procedure "proc_make_string"))
    (define make-vector (bind-procedure "proc_make_vector"))
    (define map (bind-procedure "proc_map"))
    (define max (bind-procedure "proc_max"))
    (define member (bind-procedure "proc_member"))
    (define memq (bind-procedure "proc_memq"))
    (define memv (bind-procedure "proc_memv"))
    (define min (bind-procedure "proc_min"))
    (define modulo (bind-procedure "proc_modulo"))
    (define nan?  (bind-procedure "proc_nanp"))
    (define negative?  (bind-procedure "proc_negativep"))
    (define newline (bind-procedure "proc_newline"))
    (define not (bind-procedure "proc_not"))
    (define null?  (bind-procedure "proc_nullp"))
    (define number->string (bind-procedure "proc_number_to_string"))
    (define number?  (bind-procedure "proc_numberp"))
    (define odd?  (bind-procedure "proc_oddp"))
    (define or (bind-procedure "proc_or"))
    (define output-port?  (bind-procedure "proc_output_portp"))
    (define pair?  (bind-procedure "proc_pairp"))
    (define port-open?  (bind-procedure "proc_port_openp"))
    (define port?  (bind-procedure "proc_portp"))
    (define positive?  (bind-procedure "proc_positivep"))
    (define procedure?  (bind-procedure "proc_procedurep"))
    (define quasiquote (bind-syntax "proc_dummy_placeholder"))
    (define quote (bind-syntax "proc_dummy_placeholder"))
    (define real?  (bind-procedure "proc_realp"))
    (define reverse (bind-procedure "proc_reverse"))
    (define round (bind-procedure "proc_round"))
    (define set!  (bind-syntax "proc_dummy_placeholder"))
    (define set-car!  (bind-syntax "proc_dummy_placeholder"))
    (define set-cdr!  (bind-syntax "proc_dummy_placeholder"))
    (define string (bind-procedure "proc_string"))
    (define string->list (bind-procedure "proc_string_to_list"))
    (define string->number (bind-procedure "proc_string_to_number"))
    (define string->symbol (bind-procedure "proc_string_to_symbol"))
    (define string->vector (bind-procedure "proc_string_to_vector"))
    (define string-append (bind-procedure "proc_strcat"))
    (define string-length (bind-procedure "proc_string_length"))
    (define string-ref (bind-procedure "proc_string_ref"))
    (define string<=?  (bind-procedure "proc_string_ltep"))
    (define string<?  (bind-procedure "proc_string_ltp"))
    (define string=?  (bind-procedure "proc_string_eqp"))
    (define string>=?  (bind-procedure "proc_string_gtep"))
    (define string>?  (bind-procedure "proc_string_gtp"))
    (define string?  (bind-procedure "proc_stringp"))
    (define substring (bind-procedure "proc_substring"))
    (define symbol->string (bind-procedure "proc_symbol_to_string"))
    (define symbol?  (bind-procedure "proc_symbolp"))
    (define textual-port?  (bind-procedure "proc_textual_portp"))
    (define truncate (bind-procedure "proc_truncate"))
    (define vector (bind-procedure "proc_vector"))
    (define vector->list (bind-procedure "proc_vector_to_list"))
    (define vector->string (bind-procedure "proc_vector_to_string"))
    (define vector-copy (bind-procedure "proc_vector_copy"))
    (define vector-fill!  (bind-procedure "proc_vector_fill"))
    (define vector-length (bind-procedure "proc_vector_length"))
    (define vector-ref (bind-procedure "proc_vector_ref"))
    (define vector-set!  (bind-procedure "proc_vector_set"))
    (define vector?  (bind-procedure "proc_vectorp"))
    (define write (bind-procedure "proc_write"))
    (define xor (bind-procedure "proc_xor"))
    (define zero?  (bind-procedure "proc_zerop"))

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

    ;; Code taken from R7RS draft
    (define (values . things)
      (call-with-current-continuation
        (lambda (cont) (apply cont things)))))
