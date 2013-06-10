;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (mem eq-op? needle haystack)
  (cond
    ((null? haystack) #f)
    ((eq-op? needle (car haystack)) haystack)
    (else
      (mem eq-op? needle (cdr haystack)))))

(define (member needle haystack)
  (mem equal? needle haystack))

(define (memq needle haystack)
  (mem eq? needle haystack))

(define (memv needle haystack)
  (mem eqv? needle haystack))
