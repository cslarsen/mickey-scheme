#|
   SRFI-8 for Mickey Scheme
   http://srfi.schemers.org/srfi-8/srfi-8.html

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the GNU LGPL 2.1; see LICENSE
|#
(define-library (srfi 8)
  (import (scheme base))
  (export receive)
  (begin

    ;; Bind all (values ...) from <expression> to <formals> and execute in
    ;; <body>.  This is just a very thin wrapper around call-with-values.
    ;;
    (define-syntax receive
      (syntax-rules ()
        ((receive formals expression body ...)
         (call-with-values (lambda () expression)
                           (lambda formals body ...)))))))
