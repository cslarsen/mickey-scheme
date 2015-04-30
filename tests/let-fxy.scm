(import
  (scheme base)
  (scheme write)
  (test unit-test))

(define (square x)
  (* x x))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(testq (f 10 20) 399811)
(tap-results)
