;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define (alist-find match? needle haystack)
  (cond
    ((null? haystack) #f)
    ((not (pair? (car haystack))) (error "Not an a-list"))
    ((match? needle (caar haystack)) (car haystack))
    (else
      (alist-find match? needle (cdr haystack)))))

(define (assoc needle haystack)
  (alist-find equal? needle haystack))

(define (assq needle haystack)
  (alist-find eq? needle haystack))

(define (assv needle haystack)
  (alist-find eqv? needle haystack))
