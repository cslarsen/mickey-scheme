;; Example of SKI combinator calculus,
;; http://en.wikipedia.org/wiki/SKI_combinator_calculus
;; 
;; In other words, the functions below can be used to
;; run a universal programming language on top of the
;; Lisp it runs on.
;;
;; All you need to get this to work is lambda, closures
;; and currying.

; Identity combinator
(define I
  (lambda (x) x))

; Constant combinator
(define K
  (lambda (x)
    (lambda (y) x)))

; Substitution combinator (is prolly wrong at the moment)
(define S
  (lambda (x y z)
    ((y z) (x z))))


