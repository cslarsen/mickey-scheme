;; Shows Mickey Scheme R7RS compliance level
;; Based on R7RS-small draft 10

;; Remember to import all libraries
(import (portable print)
        (mickey environment)
        (scheme base)
        (scheme case-lambda)
        (scheme char)
        (scheme complex)
        (scheme cxr)
        (scheme eval)
        (scheme file)
        (scheme inexact)
        (scheme lazy)
        (scheme load)
        (scheme process-context)
        (scheme r5rs)
        (scheme read)
        (scheme repl)
        (scheme time)
        (scheme write))

;; A-list of all standard R7RS modules and their exported identifiers.
;;
(define libraries
  '(((scheme base) (*
                    +
                    -
                    /
                    <
                    <=
                    =
                    =>
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
                    boolean=?
                    boolean?
                    bytevector
                    bytevector-append
                    bytevector-copy
                    bytevector-copy!
                    bytevector-length
                    bytevector-u8-ref
                    bytevector-u8-set!
                    bytevector?
                    caar
                    cadr
                    call-with-current-continuation
                    call-with-port
                    call-with-values
                    call/cc
                    car
                    case
                    cdar
                    cddr
                    cdr
                    ceiling
                    char->integer
                    char-ready?
                    char<=?
                    char<?
                    char=?
                    char>=?
                    char>?
                    char?
                    close-input-port
                    close-output-port
                    close-port
                    complex?
                    cond
                    cond-expand
                    cons
                    current-error-port
                    current-input-port
                    current-output-port
                    define
                    define-record-type
                    define-syntax
                    define-values
                    denominator
                    do
                    dynamic-wind
                    else
                    eof-object
                    eof-object?
                    eq?
                    equal?
                    eqv?
                    error
                    error-object-irritants
                    error-object-message
                    error-object?
                    even?
                    exact
                    exact-integer-sqrt
                    exact-integer?
                    exact?
                    expt
                    features
                    file-error?
                    floor
                    floor-quotient
                    floor-remainder
                    floor/
                    flush-output-port
                    for-each
                    gcd
                    get-output-bytevector
                    get-output-string
                    guard
                    if
                    include
                    include-ci
                    inexact
                    inexact?
                    input-port-open?
                    input-port?
                    integer->char
                    integer?
                    lambda
                    lcm
                    length
                    let
                    let*
                    let*-values
                    let-syntax
                    let-values
                    letrec
                    letrec*
                    letrec-syntax
                    list
                    list->string
                    list->vector
                    list-copy
                    list-ref
                    list-set!
                    list-tail
                    list?
                    make-bytevector
                    make-list
                    make-parameter
                    make-string
                    make-vector
                    map
                    max
                    member
                    memq
                    memv
                    min
                    modulo
                    negative?
                    newline
                    not
                    null?
                    number->string
                    number?
                    numerator
                    odd?
                    open-input-bytevector
                    open-input-string
                    open-output-bytevector
                    open-output-string
                    or
                    output-port-open?
                    output-port?
                    pair?
                    parameterize
                    peek-char
                    peek-u8
                    port?
                    positive?
                    procedure?
                    quasiquote
                    quote
                    quotient
                    raise
                    raise-continuable
                    rational?
                    rationalize
                    read-bytevector
                    read-bytevector!
                    read-char
                    read-error?
                    read-line
                    read-string
                    read-u8
                    real?
                    remainder
                    reverse
                    round
                    set!
                    set-car!
                    set-cdr!
                    square
                    string
                    string->list
                    string->number
                    string->symbol
                    string->utf8
                    string->vector
                    string-append
                    string-copy
                    string-copy!
                    string-fill!
                    string-for-each
                    string-length
                    string-map
                    string-ref
                    string-set!
                    string<=?
                    string<?
                    string=?
                    string>=?
                    string>?
                    string?
                    substring
                    symbol->string
                    symbol=?
                    symbol?
                    syntax-error
                    syntax-rules
                    textual-port?
                    truncate
                    truncate-quotient
                    truncate-remainder
                    truncate/
                    u8-ready?
                    unless
                    unquote
                    unquote-splicing
                    utf8->string
                    values
                    vector
                    vector->list
                    vector->string
                    vector-append
                    vector-copy
                    vector-copy!
                    vector-fill!
                    vector-for-each
                    vector-length
                    vector-map
                    vector-ref
                    vector-set!
                    vector?
                    when
                    with-exception-handler
                    write-bytevector
                    write-char
                    write-string
                    write-u8
                    zero?))

    ((scheme case-lambda) (case-lambda))

    ((scheme char) (char-alphabetic?
                    char-ci<=?
                    char-ci<?
                    char-ci=?
                    char-ci>=?
                    char-ci>?
                    char-downcase
                    char-foldcase
                    char-lower-case?
                    char-numeric?
                    char-upcase
                    char-upper-case?
                    char-whitespace?
                    digit-value
                    string-ci<=?
                    string-ci<?
                    string-ci=?
                    string-ci>=?
                    string-ci>?
                    string-downcase
                    string-foldcase
                    string-upcase))

    ((scheme complex) (angle
                       imag-part
                       magnitude
                       make-polar
                       make-rectangular
                       real-part))

    ((scheme cxr) (caaar
                   caadr
                   cadar
                   caddr
                   cdaar
                   cdadr
                   cddar
                   cdddr
                   caaaar
                   caaadr
                   caadar
                   caaddr
                   cadaar
                   cadadr
                   caddar
                   cadddr
                   cdaaar
                   cdaadr
                   cdadar
                   cdaddr
                   cddaar
                   cddadr
                   cdddar
                   cddddr))

    ((scheme eval) (environment
                    eval))

    ((scheme file) (call-with-input-file
                    call-with-output-file
                    delete-file
                    file-exists?
                    open-binary-input-file
                    open-binary-output-file
                    open-input-file
                    open-output-file
                    with-input-from-file
                    with-output-to-file))

    ((scheme inexact) (acos
                       asin
                       atan
                       cos
                       exp
                       finite?
                       infinite?
                       log
                       nan?
                       sin
                       sqrt
                       tan))

    ((scheme lazy) (delay
                    delay-force
                    force
                    make-promise
                    promise?))

    ((scheme load) (load))

    ((scheme process-context) (command-line
                               emergency-exit
                               exit
                               get-environment-variable
                               get-environment-variables))

    ((scheme read) (read))

    ((scheme repl) (interaction-environment))

    ((scheme time) (current-jiffy
                    current-second
                    jiffies-per-second))

    ((scheme write) (display
                     write
                     write-shared
                     write-simple))

    ((scheme r5rs) (*
                    +
                    -
                    /
                    <
                    <=
                    =
                    >
                    >=
                    abs
                    acos
                    and
                    angle
                    append
                    apply
                    asin
                    assoc
                    assq
                    assv
                    atan
                    begin
                    boolean?
                    caaaar
                    caaadr
                    caaar
                    caadar
                    caaddr
                    caadr
                    caar
                    cadaar
                    cadadr
                    cadar
                    caddar
                    cadddr
                    caddr
                    cadr
                    call-with-current-continuation
                    call-with-input-file
                    call-with-output-file
                    call-with-values
                    car
                    case
                    cdaaar
                    cdaadr
                    cdaar
                    cdadar
                    cdaddr
                    cdadr
                    cdar
                    cddaar
                    cddadr
                    cddar
                    cdddar
                    cddddr
                    cdddr
                    cddr
                    cdr
                    ceiling
                    char->integer
                    char-alphabetic?
                    char-ci<=?
                    char-ci<?
                    char-ci=?
                    char-ci>=?
                    char-ci>?
                    char-downcase
                    char-lower-case?
                    char-numeric?
                    char-ready?
                    char-upcase
                    char-upper-case?
                    char-whitespace?
                    char<=?
                    char<?
                    char=?
                    char>=?
                    char>?
                    char?
                    close-input-port
                    close-output-port
                    complex?
                    cond
                    cons
                    cos
                    current-input-port
                    current-output-port
                    define
                    define-syntax
                    delay
                    denominator
                    display
                    do
                    dynamic-wind
                    eof-object?
                    eq?
                    equal?
                    eqv?
                    eval
                    even?
                    exact->inexact
                    exact?
                    exp
                    expt
                    floor
                    for-each
                    force
                    gcd
                    if
                    imag-part
                    inexact->exact
                    inexact?
                    input-port?
                    integer->char
                    integer?
                    interaction-environment
                    lambda
                    lcm
                    length
                    let
                    let*
                    let-syntax
                    letrec
                    letrec-syntax
                    list
                    list->string
                    list->vector
                    list-ref
                    list-tail
                    list?
                    load
                    log
                    magnitude
                    make-polar
                    make-rectangular
                    make-string
                    make-vector
                    map
                    max
                    member
                    memq
                    memv
                    min
                    modulo
                    negative?
                    newline
                    not
                    null-environment
                    null?
                    number->string
                    number?
                    numerator
                    odd?
                    open-input-file
                    open-output-file
                    or
                    output-port?
                    pair?
                    peek-char
                    positive?
                    procedure?
                    quasiquote
                    quote
                    quotient
                    rational?
                    rationalize
                    read
                    read-char
                    real-part
                    real?
                    remainder
                    reverse
                    round
                    scheme-report-environment
                    set!
                    set-car!
                    set-cdr!
                    sin
                    sqrt
                    string
                    string->list
                    string->number
                    string->symbol
                    string-append
                    string-ci<=?
                    string-ci<?
                    string-ci=?
                    string-ci>=?
                    string-ci>?
                    string-copy
                    string-fill!
                    string-length
                    string-ref
                    string-set!
                    string<=?
                    string<?
                    string=?
                    string>=?
                    string>?
                    string?
                    substring
                    symbol->string
                    symbol?
                    tan
                    truncate
                    values
                    vector
                    vector->list
                    vector-fill!
                    vector-length
                    vector-ref
                    vector-set!
                    vector?
                    with-input-from-file
                    with-output-to-file
                    write
                    write-char
                    zero?))))

(define (boolean->integer b)
  (if b 1 0))

(define (log10 n)
  "Return the base-10 logarithm of n."
  (/ (log n) (log 10)))

(define (digits n)
  "Returns number of digits in n."
  (cond
    ((zero? n) 1)
    ((negative? n) (+ 1 (digits (- n))))
    (else
      (+ 1 (exact (truncate (log10 n)))))))

(define (spaces obj)
  (cond
    ((number? obj) (digits obj))
    ((string? obj) (string-length obj))
    (else (error "spaces: unknown object type"))))

(define (right-align width obj)
  (let
    ((chars (spaces obj)))
    (string-append
      (make-string (abs (- width chars)))
      (cond
        ((number? obj) (number->string obj))
        ((string? obj) obj)
        (else (error "Unknown object type"))))))

(define (main)
  (define *total* 0)
  (define *bound* 0)
  (define *missing* 0)

  (println
    "The table below shows how many of the definitions in R7RS-small that have\n"
    "been implemented in Mickey Scheme.\n"
    "\n"
    "The first number shows the coverage in percent, then number of implemented\n"
    "definitions, definitions required by R7RS-small, missing definitions and\n"
    "name of the library.\n")

  (for-each
    (lambda (library symbols)
      (let*
        ((total (length symbols))
         (bound (apply + (map boolean->integer
                              (map bound? symbols))))
         (missing (- total bound))
         (percent (round (* 100 (/ bound total)))))

        (set! *total* (+ *total* total))
        (set! *bound* (+ *bound* bound))
        (set! *missing* (+ *missing* missing))

        (println (right-align 7 percent) "% "
                 (right-align 7
                   (string-append
                     (number->string bound) "/"
                     (number->string total))) " "
                 (right-align 4 (- missing)) " "
                 library)))
    (map car libraries)
    (map cadr libraries))

  (define *percent* (round (* 100 (/ *bound* *total*))))

  (println (right-align 7 *percent*) "% "
           (right-align 7
             (string-append
               (number->string *bound*) "/"
               (number->string *total*))) " "
           (right-align 4 (- *missing*)) " <all>")

  (println "\n"
           "In summary, Mickey implements " *bound* " of "
           *total* " definitions in R7RS-small.\n"
           *missing* " definitions have not been implemented.\n"
           "\n"
           "This corresponds to " *percent* "% coverage."))

(main)
