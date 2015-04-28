;; The Mandelbrot, printed in ASCII
;; Straight-forward version, with no optimizations

;; Functions for COMPLEX NUMBERS

(define make-complex
  (lambda (re im)
    (cons re im)))

; Real part
(define re
  (lambda (z)
    (car z)))

; Imaginary part
(define im
  (lambda (z)
    (cdr z)))

(define +complex
  (lambda (a b)
    (make-complex
      (+ (re a) (re b))
      (+ (im a) (im b)))))

(define -complex
  (lambda (a b)
    (make-z
      (- (re a) (re b))
      (- (im a) (im b)))))

(define *complex
  ; (a + ib) * (c + id) = (ac + i(ad + ibc) - bd)
  ;                     = (ac - bd) + i(ad + bc)
  (lambda (a b)
    (make-complex
      (- (* (re a) (re b))  ; ac
         (* (im a) (im b))) ; bd
      (+ (* (re a) (im b))     ; ad
         (* (im a) (re b)))))) ; bc

; Magnitude
(define complex-mag
  (lambda (z)
    (sqrt (+ (* (re z) (re z))
             (* (im z) (im z))))))

(define complex->string
  (lambda (z)
    (string-append
      (number->string (re z)) " + "
      (number->string (im z)) "i")))
