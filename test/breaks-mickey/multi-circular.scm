;; Both Mickey and Chicken report that x is a list...
;; because, it IS... it does not end with a nil, but again,
;; it is circular.

;; Chicken also has an iterative tail-call friendly printing
;; facility, it seems.
;;
;; Mickey Scheme's sprint() is not iterative and therefore
;; breaks badly when trying to display x.

(define y (list 'a 'b))
(define x (list 1 2 y 3))
(set-cdr! y x)

(display "list? ")
(display (list? x))
(newline)

(display "x: ")
(display x)
(newline)

(display "Well, that worked nicely!")
(newline)
