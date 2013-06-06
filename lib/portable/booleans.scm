;;
;;  Copyright (C) 2013 Christian Stigen Larsen
;;
;;  Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
;;  See the file LICENSE for full text.
;;
;;  Provides boolean logic operators such as boolean/and and friends, made
;;  to be compatible with MIT Scheme.
;;
;;  It's a known problem in Scheme that one cannot apply "and", because it
;;  is a macro.  So this does not work:
;;
;;     (apply and (list #t #t #t))
;;
;;  This module provides a procedural (and therefore eagerly evaluated)
;;  boolean/and procedure, so one can do:
;;
;;     (apply boolean/and (list #t #t #t))
;;
;;
(define-library (portable booleans)
  (import
    (scheme base))

  (export
    boolean/and
    boolean/or
    boolean=?
    false
    false?
    true
    true?)

  (begin

    ;; Constant that evaluates to #t.
    (define true
      '#t)

    ;; Constant that evaluates to #f.
    (define false
      '#f)

    ;; Return #t if "x" is false.
    ;;
    (define (false? x)
      (if x #f #t))

    ;; Return #t if "x" is true.
    ;;
    (define (true? x)
      (if x #t #f))

    ;; Procedural version of "and".
    ;;
    (define (boolean/and . x)
      (cond
        ((null? x) #t)
        ((car x) (apply boolean/and (cdr x)))
        (else #f)))

    ;; Procedural version of "or".
    ;;
    (define (boolean/or . x)
      (cond
        ((null? x) #f)
        ((car x) #t)
        (else (apply boolean/or (cdr x)))))

    ;; True iff "a" and "b" are either both true or both false.
    ;; TODO: Make procedure variadic.
    ;;
    (define (boolean=? a b)
      (true? (or (and a b)
                 (and (not a)
                      (not b)))))
))
