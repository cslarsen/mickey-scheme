;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (make-list k . fill)
  (if (null? fill)
    (vector->list (make-vector k #f))
    (if (not (= 1 (length fill)))
      (error "make-list takes exactly one or two parameters")
      (vector->list (apply make-vector (cons k fill))))))
