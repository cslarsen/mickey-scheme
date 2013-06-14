;;
;;  Linear Congruential Generator (LCG) for Mickey Scheme.
;;  Part of the pseudo-random number generator (PRNG) library.
;;
;;  Copyright (C) 2013 Christian Stigen Larsen
;;
;;  Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
;;  See the file LICENSE for detailed information.
;;
;;  For more information on LCGs, see either
;;
;;    - Wikipedia,
;;      https://en.wikipedia.org/wiki/Linear_congruential_generator 
;;
;;    - Knuth's The Art of Computer Programming (TAOCP), volume 2, starting
;;      on page 10.
;;
;;  Wikipedia lists some common parameters, and most of these have been
;;  reproduced below.  But some even more interesting ones can be found in
;;  TAOCP, vol. 2, on pages 106 and 107 (e.g., the Borosh-Niederreiter
;;  multiplier, which is also used to seed the initial state in the Mersenne
;;  Twister PRNG).
;;
;;  IMPORTANT
;;
;;  Mickey currently only supports fixnum integers, so the numerical range
;;  too limited to be able to handle most of the LCGs in this library.
;;  You'll get negative values, or the cond case below will signal an error.
;;
;;  EXAMPLE USAGE
;;
;;  #; mickey> (import (portable prng lcg))
;;  #; mickey> (define random (lcg-simple 123))
;;  #; mickey> (random)
;;  142
;;  #; mickey> (random)
;;  185
;;  #; mickey> (random)
;;  188
;;  #; mickey> (random)
;;  87
;;
(define-library (portable prng lcg)
  (import (scheme base)
          (scheme case-lambda))

  (export
    lcg-carbonlib
    lcg-forth
    lcg-glibc
    lcg-java
    lcg-mmix
    lcg-newlib
    lcg-simple
    lcg-vb6
    make-lcg)

  (begin

    ;; Creates an LCG with given initial values:
    ;;
    ;;   m, the modulus
    ;;   a, the multiplier
    ;;   c, the increment
    ;;   seed, the start value
    ;;
    (define make-lcg
      (case-lambda
        ((m a c) (make-lcg m a c 0)) ; default seed of 0
        ((m a c seed)
         (cond
           ((not (and (number? m)
                      (number? a)
                      (number? c)
                      (number? seed)))
            (error "All parameters must be numbers"))

           ((not (< 0 m))
            (error (string-append
                     "Modulus m must be greater than zero: "
                     (number->string m))))

           ((not (< 0 a m))
            (error (string-append
                     "Multiplier a must be in range (0,modulus): "
                     (number->string a))))

           ((not (and (<= 0 c) (< c m)))
            (error (string-append
                     "Increment c must be in range [0,modulus): "
                     (number->string c))))

           ((not (and (<= 0 seed) (< seed m)))
            (error (string-append
                     "Seed must be in range [0,modulus): "
                     (number->string seed)))))
         (let
           ((X seed))
           (lambda ()
             (set! X (modulo (+ (* a X) c) m))
             X)))))

    ; Simple, with modulus and multiplier taken from TAOCP, vol 2, page 106,
    ; line 5, with an increment of 187, as given in figure 8, p. 94 (ibid.)
    (define (lcg-simple seed)
      (make-lcg 256 137 187 seed))

    ; Full GNU glibc
    (define (lcg-glibc seed)
      (make-lcg (expt 2 31)
                1103515245
                12345
                seed))

    ; Knuth's MMIX
    (define (lcg-mmix seed)
      (make-lcg (expt 2 64)
                6364136223846793005
                1442695040888963407
                seed))

    ; Java's java.util.Random
    (define (lcg-java seed)
      (make-lcg (expt 2 48)
                25214903917
                11
                seed))

    ; Forth's LC53
    (define (lcg-forth seed)
      (make-lcg (- (expt 2 32 5))
                (- (expt 2 32) 333333333)
                0
                seed))

    ; Newlib C library
    (define (lcg-newlib seed)
      (make-lcg (expt 2 64)
                6364136223846793005
                1
                seed))

    ; Microsoft Visual Basic 6 and earlier
    (define (lcg-vb6 seed)
      (make-lcg (expt 2 24)
                #x43FD43FD
                #xC39EC3
                seed))

    ; Apple CarbonLib
    (define (lcg-carbonlib seed)
      (make-lcg (- (expt 2 31) 1)
                #x7FFFFFED
                #x7FFFFFC3
                seed))))
