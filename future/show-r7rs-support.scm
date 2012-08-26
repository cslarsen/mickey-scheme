;; Show which R7RS definitions are available in Mickey Scheme.
(import (scheme base)
        (scheme write)
        (mickey environment))

(define base-lib '(
  *
  +
  -
; ... ; literal used by syntax-rules
  /
  <
  <=
  =
; => ; literal used by (cond)
  >
  >=
; _ ; literal used by syntax-rules
  abs
  and
  append
  apply
  assoc
  assq
  assv
  begin
  binary-port?
  boolean?
  bytevector-copy
  bytevector-copy!
  bytevector-copy-partial
  bytevector-copy-partial!
  bytevector-length
  bytevector-u8-ref
  bytevector-u8-set!
  bytevector?
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
  cadr
  call-with-current-continuation
  call-with-port
  call-with-values
  call/cc
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
; else ; literal
  eof-object?
  eq?
  equal?
  eqv?
  error
  error-object-irritants
  error-object-message
  error-object?
  even?
  exact->inexact
  exact-integer-sqrt
  exact-integer?
  exact?
  expt
  floor
  flush-output-port
  for-each
  gcd
  get-output-bytevector
  get-output-string
  guard
  if
  import
  inexact->exact
  inexact?
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
  list-string
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
  output-port?
  pair?
  parameterize
  peek-char
  peek-u8
  port-open?
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
  read-line
  read-u8
  real?
  remainder
  reverse
  round
  set!
  set-car!
  set-cdr!
  string
  string->list
  string->number
  string->symbol
  string->utf8
  string->vector
  string-append
  string-copy
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
  symbol?
  syntax-error
  syntax-rules
  textual-port?
  t4runcate
  u8-ready?
  unless
  unquote
  unquote-splicing
  utf8->string
  values
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
  with-exception-handler
  write-bytevector
  write-char
  write-partial-bytevector
  write-u8
  zero?))

(define inexact-lib '(
  acos
  asin
  atan
  cos
  exp
  finite?
  log
  nan?
  sin
  sqrt
  tan
))

(define complex-lib '(
  angle
  imag-part
  magnitude
  make-polar
  make-rectangular
  real-part
))

(define division-lib '(
  ceiling-quotient
  ceiling-remainder
  ceiling/
  centered-quotient
  centered-remainder
  centered/
  euclidean-quotient
  euclidean-remainder
  euclidean/
  floor-quotient
  floor-remainder
  floor/
  round-quotient
  round-remainder
  round/
  truncate-quotient
  truncate-remainder
  truncate/
))

(define lazy-lib '(delay eager force lazy))

(define case-lambda-lib '(case-lambda))

(define eval-lib '(
  environment
  eval
  null-environment
  scheme-report-environment))

(define repl-lib '(interaction-environment))

(define process-context-lib '(
  command-line
  exit
  get-environment-variable
  get-environment-variables
))

(define load-lib '(load))

(define file-lib '(
  call-with-input-file
  call-with-output-file
  delete-file
  file-exists?
  open-binary-input-file
  open-binary-output-file
  open-input-file
  open-output-file
  with-input-from-file
  with-output-to-file
))

(define read-lib '(read))

(define write-lib '(
  display
  write
  write-simple
))

(define char-lib '(
  char-alphabetic?
  char-ci<=?
  char-ci<?
  char-ci=?
  char-ci>=?
  char-ci>?
  char-downcase
  char-foldcase
  char-lower-case?
  char-numeric?
  char-upcase?
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
  string-upcase
))

(define char-normalization-lib '(
  string-ni<=?
  string-ni<?
  string-ni=?
  string-ni>=?
  string-ni>?
))

(define time-lib '(
  current-jiffy
  current-second
  jiffies-per-second
))

(define (show-bound-definitions library-name definitions)
  (map
    (lambda (sym)
      (display (string-append
        library-name " "
        (if (bound? sym) "supported: " "missing: ")
        sym "\n")))
    definitions))

(display "The following list shows the R7RS library definitions")
(display "that are available in Mickey Scheme.")
(newline)

(show-bound-definitions "(scheme base)" base-lib)
(show-bound-definitions "(scheme inexact)" inexact-lib)
(show-bound-definitions "(scheme complex)" complex-lib)
(show-bound-definitions "(scheme division)" division-lib)
(show-bound-definitions "(scheme lazy)" lazy-lib)
(show-bound-definitions "(scheme case-lambda)" case-lambda-lib)
(show-bound-definitions "(scheme eval)" eval-lib)
(show-bound-definitions "(scheme repl)" repl-lib)
(show-bound-definitions "(scheme process-context)" process-context-lib)
(show-bound-definitions "(scheme load)" load-lib)
(show-bound-definitions "(scheme file)" file-lib)
(show-bound-definitions "(scheme read)" read-lib)
(show-bound-definitions "(scheme write)" write-lib)
(show-bound-definitions "(scheme char)" char-lib)
(show-bound-definitions "(scheme char-normalization)" char-normalization-lib)
(show-bound-definitions "(scheme time)" time-lib)

