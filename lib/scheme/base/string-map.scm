;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (string-map proc . lists)
  (let
    ((lists (map string->list lists)))
    (list->string
      (apply map (cons proc lists)))))

