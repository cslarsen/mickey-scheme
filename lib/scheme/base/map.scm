;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; TODO:
;; - Make map and mapcar tail-recursive; use accumulators
;; - Do not use (if = (length ...) ...) check, which
;;   is incredibly bad code.

(define (map proc . lists)
  (define (mapcar proc items)
    (if (or (null? items)
            (null? (car items)))
      '()
      (cons (proc (car items))
            (mapcar proc (cdr items)))))

  (let
    ((count (length lists)))
    (if (null? (car lists)) '()
      (let
        ((heads (mapcar car lists))
         (tails (mapcar cdr lists)))
        (if (= (length heads) count) ; SLOW CHECK
          (cons
            (apply proc heads)
            (apply map (cons proc tails)))
          '())))))
