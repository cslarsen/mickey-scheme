;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (string-for-each proc . lists)
  (let
    ((lists (map string->list lists)))
    (apply for-each (cons proc lists))))

