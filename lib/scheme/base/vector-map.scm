;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; TODO: Use vector-ref instead of vector->List,
;;       which is faster

(define (vector-map proc . lists)
  (let
    ((lists (map vector->list lists)))
    (list->vector
      (apply map (cons proc lists)))))

