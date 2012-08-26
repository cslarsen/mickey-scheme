;; The Mandelbrot set in ASCII
;;
;; Calculated in a straight-forward manner,
;; without any sort of optimizations whatsoever.
;;
;; This program thus serves as a good yardstick
;; for benchmarking the natural performance of various
;; Schemes.
;;
;; Note that this version is tailored for Mickey Schee,
;; meaning that it actually uses a subset of RnRS Scheme.
;;
;; It's been verified to work with Chicken Scheme.
;;
;; Made by Christian Stigen Larsen in 2011
;; Placed in the public domain.

(import (scheme base)
        (scheme write)
        (scheme math)
        (scheme load))

;; Number of iterations, increase for more accurate
;; answer at a cost of slower computation
(define cutoff-steps 20)

;; Output screen sizes
(define screen-columns 78)
(define screen-rows    25)

;; Characters used for drawing
(define dot-char   "*")
(define space-char " ")

(load "complex.scm")

(define (<= x y)
  (or (= x y)
      (< x y)))

(define (1- n)
  (- n 1))

; C_{n+1} = C_{n}^2  + c, C_{0} = c
(define (mandelbrot?-iter z c nmax)
  (if (zero? nmax) #f
  (if (> (complex-mag z) threshold) #t
      (mandelbrot?-iter
        (+complex c (*complex z z))
                  c (1- nmax)))))

(define (mandelbrot? z)
  (mandelbrot?-iter
    (make-complex 0.0 0.0)
     z cutoff-steps))

(define (plot x y)
  (display (string-append
    (if (mandelbrot? (make-complex x y))
        space-char dot-char))))

;; Main

(define threshold 2.0) ;; should be fixed

(define x-start -2.0)
(define x-stop   1.0)
(define x-step   (/ (- x-stop x-start) (+ 0.5 (- screen-columns 1))))

(define y-start -1.0)
(define y-stop   1.0)
(define y-step   (/ (- y-stop y-start) (- screen-rows 1)))

(define (x-loop x y)
  (if (not (<= x x-stop)) 0
      (begin
        (plot x y)
        (x-loop (+ x x-step) y))))

(define (y-loop y)
  (if (not (<= y y-stop)) 0
      (begin
        (x-loop x-start y)
        (display "\n")
        (y-loop (+ y y-step)))))

(y-loop y-start)
