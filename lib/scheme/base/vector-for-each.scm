;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; TODO: Use vector-ref instead for faster speed.

(define (vector-for-each proc . lists)
  (let
    ((lists (map vector->list lists)))
    (apply for-each (cons proc lists))))

