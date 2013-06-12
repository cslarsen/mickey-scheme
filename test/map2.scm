(import (scheme base)
        (scheme write))

;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; TODO:
;; - Make map and mapcar tail-recursive; use accumulators
;; - Do not use (if = (length ...) ...) check, which
;;   is incredibly bad code.

(define (mmap proc . lists)
  (let
    ((count (length lists)))

    (define (mapcar proc items)
      (if (or (null? items)
              (null? (car items)))
        '()
        (cons (proc (car items))
              (mapcar proc (cdr items)))))

    (if (null? (car lists)) '()
      (let
        ((heads (mapcar car lists))
         (tails (mapcar cdr lists)))

        (if (= (length heads) count) ; SLOW CHECK
          (cons
            (apply proc heads)
            (apply mmap (cons proc tails)))
          '())))))

(display "Expected output: ((1 a) (2 b))")
(newline)

(display "Actual output  : ")
(display (mmap list '(1 2 3) '(a b)))
(newline)

(display "Should stop when first list runs out.")
(newline)
