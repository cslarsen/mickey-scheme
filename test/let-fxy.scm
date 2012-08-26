(import (scheme base))
(import (scheme write))

(define (square x) (* x x))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(display (f 10 20)) ; should be 399811
(newline)
