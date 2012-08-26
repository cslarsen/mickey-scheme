#|

Mickey R7RS Scheme

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE
Please post bugfixes and suggestions to the author.

|#
(define-library (scheme cxr)
  (import (only (scheme base) car cdr))

  (export
    caaar  caadr  cadar  caddr
    cdaar  cdadr  cddar  cdddr
    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr)

  (begin
    (define (caaar s) (car (car (car s))))
    (define (caadr s) (car (car (cdr s))))
    (define (cadar s) (car (cdr (car s))))
    (define (caddr s) (car (cdr (cdr s))))
    (define (cdaar s) (cdr (car (car s))))
    (define (cdadr s) (cdr (car (cdr s))))
    (define (cddar s) (cdr (cdr (car s))))
    (define (cdddr s) (cdr (cdr (cdr s))))

    (define (caaaar s) (car (car (car (car s)))))
    (define (caaadr s) (car (car (car (cdr s)))))
    (define (caadar s) (car (car (cdr (car s)))))
    (define (caaddr s) (car (car (cdr (cdr s)))))
    (define (cadaar s) (car (cdr (car (car s)))))
    (define (cadadr s) (car (cdr (car (cdr s)))))
    (define (caddar s) (car (cdr (cdr (car s)))))
    (define (cadddr s) (car (cdr (cdr (cdr s)))))
    (define (cdaaar s) (cdr (car (car (car s)))))
    (define (cdaadr s) (cdr (car (car (cdr s)))))
    (define (cdadar s) (cdr (car (cdr (car s)))))
    (define (cdaddr s) (cdr (car (cdr (cdr s)))))
    (define (cddaar s) (cdr (cdr (car (car s)))))
    (define (cddadr s) (cdr (cdr (car (cdr s)))))
    (define (cdddar s) (cdr (cdr (cdr (car s)))))
    (define (cddddr s) (cdr (cdr (cdr (cdr s)))))))
