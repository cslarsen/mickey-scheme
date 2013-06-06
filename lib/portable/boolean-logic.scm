#|

   Provides boolean logic operators.

   Copyright (C) 2013 Christian Stigen Larsen

   Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
   See the file LICENSE for full text.

   Provides boolean logic operators such as boolean/and and friends.  These
   are procedural instead of syntactic, so they will eagerly evaluate all
   their arguments.

   The reason for this module is that one cannot do

     (apply and (list #t #t #t))

   because and is a macro.  This module lets one instead use

     (apply boolean/and (list #t #t #t))

   TODO:

     - Add boolean=?

|#
(define-library (portable boolean-logic)
  (import
    (scheme base))

  (export
    boolean/and
    boolean/or)

  (begin

    ;; Procedural version of "and".
    ;;
    (define (boolean/and . x)
      (cond
        ((null? x) #t)                        ; end?   all items were true
        ((car x) (apply boolean/and (cdr x))) ; true?  next
        (else #f)))                           ; false? stop and return false

    ;; Procedural version of "or".
    ;;
    (define (boolean/or . x)
      (cond
        ((null? x) #f)                      ; end?   all items were false
        ((car x) #t)                        ; true?  stop and return true
        (else (apply boolean/or (cdr x))))) ; false? next
))
