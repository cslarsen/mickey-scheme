#|

   Simple R7RS printing library

   Copyright (C) 2012 Christian Stigen Larsen

   Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
   See the file LICENSE for full text.

|#
(define-library (portable print)
  (import (scheme base)
          (only (scheme write) newline))
  (export print println prints printsln)
  (begin
    ; Calls display on each element.
    ;
    ; Example:
    ; (print "Here are some numbers: " 1 2 3 " & a list: " '(a b c))
    ;
    (define (print . args)
      (for-each display args))

    ; Same as (print) but ends with a newline.
    (define (println . args)
      (apply print args)
      (newline))

    ; Same as (print) but adds a space between each element.
    (define (prints . args)
      (let ((n (length args)))
        (for-each (lambda (a)
                    (print a
                      (if (> n 1) " " "")) ; don't print space at end
                    (set! n (- n 1))) args)))

    ; Same as (prints) ending with a newline.
    (define (printsln . args)
      (apply prints args)
      (newline))))
