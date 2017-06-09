;; The Mandelbrot set in ASCII
;; ===========================
;;
;; Calculated in a straight-forward manner, without any sort of optimizations
;; whatsoever.
;;
;; This program thus serves as a good yardstick for benchmarking the natural
;; performance of various Schemes.
;;
;; Note that this version is tailored for Mickey Schee, meaning that it
;; actually uses a subset of RnRS Scheme.
;;
;; It's been verified to work with Chicken Scheme.
;;
;; Made by Christian Stigen Larsen in 2011
;; Placed in the public domain.

;; Expected output
;; ===============
;;
;; $ mickey examples/mandelbrot.scm
;;
;;                                                 *
;;                                               ****
;;                                              ******
;;                                    * *   *    ****  *
;;                                      ** **************** ****
;;                                     ************************
;;                                   ***************************
;;                                  ******************************
;;                     *********   *****************************
;;                    ************ *****************************
;;                    *****************************************
;; **********************************************************
;;                    *****************************************
;;                    ************ *****************************
;;                     *********   *****************************
;;                                  ******************************
;;                                   ***************************
;;                                     ************************
;;                                      ** **************** ****
;;                                    * *   *    ****  *
;;                                              ******
;;                                               ****
;;                                                 *

(import (scheme base)
        (scheme write)
        (scheme inexact)
        (scheme load))

;; Complex functions

(define (make-complex re im)
  (cons re im))

; Real part
(define (re z)
  (car z))

; Imaginary part
(define (im z)
  (cdr z))

(define (add-complex a b)
  (make-complex (+ (re a) (re b))
                (+ (im a) (im b))))

(define (multiply-complex a b)
  ; (a + ib) * (c + id) = (ac + i(ad + ibc) - bd)
  ;                     = (ac - bd) + i(ad + bc)
  (make-complex (- (* (re a) (re b))     ; ac
                   (* (im a) (im b)))    ; bd
                (+ (* (re a) (im b))     ; ad
                   (* (im a) (re b)))))  ; bc

(define (magnitude z)
  (sqrt (+ (* (re z) (re z))
           (* (im z) (im z)))))

;; The actual Mandelbrot program starts here
;; =========================================

; Number of iterations, increase for more accurate answer at a cost of slower
; computation
(define iterations 20)

; Output size
(define screen-columns 78)
(define screen-rows    25)

; Characters used for drawing
(define dot-char "*")
(define space-char " ")

(define escape-radius 2.0)

; Area of the Mandelbrot set to render
(define x-start -2.0)
(define x-stop   1.0)
(define y-start -1.0)
(define y-stop   1.0)

(define x-step   (/ (- x-stop x-start) (+ 0.5 (- screen-columns 1))))
(define y-step   (/ (- y-stop y-start) (- screen-rows 1)))

(define (mandelbrot? z)
  ; C_{n+1} = C_{n}^2  + c, C_{0} = c
  (define (mandelbrot?-iter z c nmax)
    (if (zero? nmax) #f
      (if (> (magnitude z) escape-radius) #t
          (mandelbrot?-iter
            (add-complex c (multiply-complex z z))
                         c (- nmax 1)))))
  (mandelbrot?-iter
    (make-complex 0.0 0.0)
     z iterations))

(define (plot x y)
  (display (if (mandelbrot? (make-complex x y)) space-char dot-char)))

(define (y-loop y)
  (define (x-loop x y)
    (if (not (<= x x-stop)) 0
        (begin
          (plot x y)
          (x-loop (+ x x-step) y))))
  (if (not (<= y y-stop)) 0
      (begin
        (x-loop x-start y)
        (display "\n")
        (y-loop (+ y y-step)))))

(y-loop y-start)
