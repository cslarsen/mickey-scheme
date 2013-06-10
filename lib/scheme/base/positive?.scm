;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (positive? n)
  (if (number? n)
    (> n 0)
    (error "Not a number")))
