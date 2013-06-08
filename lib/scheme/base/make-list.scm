;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (make-list k . fill)
  (if (and (not (null? fill))
           (> (length fill) 1))
    (error "make-list accepts exactly one or two parameters"))

  (let
    ((f (if (null? fill) #f
            (car fill))))

    (if (<= k 0) '()
        (cons f (make-list (- k 1) f)))))
