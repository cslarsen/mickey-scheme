;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; TODO: Make for-each and helpers tail-recursive
;; (by using accumulators)

#|
   This is a naive, straight-forward implementation.

   TODO: for-each is supposed to handle circular lists, as long as not
   all of them are circular.

   This implementation will NOT handle that situation, as it will go
   into an infinite loop, instead of raising an error.

   I think it basically means that we have to check for circularity on
   each input.
 |#
(define (for-each proc . lists)
  (define (for-each-car proc items)
    (if (not (null? items))
      (begin
        (proc (car items))
        (for-each-car proc (cdr items)))))

  (if (not (null? (car lists)))
    (let
      ((heads (map car lists))
       (tails (map cdr lists)))
        (begin
          (apply proc heads)
          (apply for-each (cons proc tails))))))
