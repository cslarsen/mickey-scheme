;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (abs n)
  (if (number? n)
    (if (negative? n) (- n) n)
    (error "Not a number")))
