;; Mickey Scheme
;;
;; Test code from SICP, taken from
;; http://mitpress.mit.edu/sicp/code/index.html

(import (scheme base)
        (scheme write)
        (scheme load))

(load "test.scm")

(display "Tests from\n")
(display "Structure and Interpretation of Comptuter Programs (SICP)\n\n")


(display "SICP SECTION 1.1.1\n")

(test-eq (quote 486) 486)
(test-eq (quote (+ 137 349)) 486)
(test-eq (quote (- 1000 334)) 666)
(test-eq (quote (* 5 99)) 495)
(test-eq (quote (/ 10 5)) 2)
(test-eq (quote (+ 2.7 10)) 12.7)
(test-eq (quote (+ 21 35 12 7)) 75)
(test-eq (quote (* 25 4 12)) 1200)
(test-eq (quote (+ (* 3 5) (- 10 6))) 19)
(test-eq (quote (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))) 57)
(test-eq (quote 
 (+ (* 3
       (+ (* 2 4)
          (+ 3 5)))
    (+ (- 10 7)
       6)))
  57) ; expected


(display "\nSICP SECTION 1.1.2\n")

(define size 2)
(test-eq (quote size) 2)
(test-eq (quote (* 5 size)) 10)

(define pi 3.14159)
(define radius 10)
(test-eq (quote (* pi (* radius radius))) 314.158997)

(define circumference (* 2 pi radius))
(test-eq (quote circumference) 62.831802)


(display "\nSICP SECTION 1.1.3\n")

(test-eq (quote (* (+ 2 (* 4 6)))) 26)
(test-eq (quote (+ 3 5 7)) 15)


(display "\nSICP SECTION 1.1.4\n")

(define (square x) (* x x))
(test-eq (quote (square 21)) 441)
(test-eq (quote (square (+ 2 5))) 49)
(test-eq (quote (square (square 3))) 81)

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(test-eq (quote (sum-of-squares 3 4)) 25)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(test-eq (quote (f 5)) 136)


(display "\nSICP SECTION 1.1.5\n")

(test-eq (quote (f 5)) 136)
(test-eq (quote (sum-of-squares (+ 5 1) (* 5 2))) 136)
(test-eq (quote (+ (square 6) (square 10))) 136)
(test-eq (quote (+ (* 6 6) (* 10 10))) 136)
(test-eq (quote (+ 36 100)) 136)

(test-eq (quote (f 5)) 136)
(test-eq (quote (sum-of-squares (+ 5 1) (* 5 2))) 136)
(test-eq (quote (+    (square (+ 5 1))      (square (* 5 2))  )) 136)
(test-eq (quote (+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))) 136)
(test-eq (quote (+         (* 6 6)             (* 10 10))) 136)
(test-eq (quote (+           36                   100)) 136)
(test-eq (quote                     136) 136)

(results)

(display "\nSICP SECTION 1.1.6\n")

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(test-eq (quote (abs  1)) 1)
(test-eq (quote (abs -1)) 1)
(test-eq (quote (abs  0)) 0)

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(test-eq (quote (abs  1)) 1)
(test-eq (quote (abs -1)) 1)
(test-eq (quote (abs  0)) 0)

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(test-eq (quote (abs  1)) 1)
(test-eq (quote (abs -1)) 1)
(test-eq (quote (abs  0)) 0)

(test-eq (quote ((lambda (x) (and (> x 5) (< x 10))) 4)) #f)
(test-eq (quote ((lambda (x) (and (> x 5) (< x 10))) 5)) #f)
(test-eq (quote ((lambda (x) (and (> x 5) (< x 10))) 6)) #t)
(test-eq (quote ((lambda (x) (and (> x 5) (< x 10))) 9)) #t)
(test-eq (quote ((lambda (x) (and (> x 5) (< x 10))) 10)) #f)
(test-eq (quote ((lambda (x) (and (> x 5) (< x 10))) 11)) #f)

(display "\nSICP SECTION 1.1.7\n")

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(test-eq (quote (number->string (sqrt 9))) "3.000092")
(test-eq (quote (sqrt (+ 100 37))) 11.704700)
(test-eq (quote (number->string (sqrt (+ (sqrt 2) (sqrt 3))))) "1.773928")
(test-eq (quote (square (sqrt 1000))) 1000.000427)

(display "\nSICP SECTION 1.1.8\n")

(define (square x) (* x x))

(define (square x) 
  (exp (double (log x))))

(define (double x) (+ x x))


;; As in 1.1.7
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))


;; Block-structured
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Taking advantage of lexical scoping
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(display "\nSICP SECTION 1.2.1\n")

;; Recursive

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;; Iterative

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Iterative, block-structured (from footnote)
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;; ----------------
;; Not part of text:
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
;; ----------------

;;EXERCISE 1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
;
;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

(display "\nSICP SECTION 1.2.2\n")

;; Recursive

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; Iterative

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; Counting change

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(test-eq (quote (count-change 100)) 292)

;;;SECTION 1.2.3

;;EXERCISE 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;;;SECTION 1.2.4

;; Linear recursion
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; Linear iteration
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

;; Logarithmic iteration
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


;;EXERCISE 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;;EXERCISE 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   ??FILL-THIS-IN?? ; compute p'
                   ??FILL-THIS-IN?? ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;;SECTION 1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;;;SECTION 1.2.6

;; prime?

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;; fast-prime?

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;;EXERCISE 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;EXERCISE 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;;EXERCISE 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;;SECTION 1.3

(define (cube x) (* x x x))

(display "\nSICP SECTION 1.3.1\n")

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;; Using sum

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

;(test-eq (quote (sum-cubes 1 10)) 1)


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

;: (sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;: (* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;(display (integral cube 0 1 0.01))

;: (integral cube 0 1 0.001)


;;EXERCISE 1.32
;: (accumulate combiner null-value term a next b)

(display "\nSICP SECTION 1.3.2\n")

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

;: ((lambda (x y z) (+ x y (square z))) 1 2 3)


;; Using let

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;: (let ((x 3)
;:       (y (+ x 2)))
;:   (* x y))

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))


;;EXERCISE 1.34
(define (f g)
  (g 2))

;: (f square)

;: (f (lambda (z) (* z (+ z 1))))


;;;SECTION 1.3.3

;; Half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


;: (half-interval-method sin 2.0 4.0)

;: (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;:                       1.0
;:                       2.0)


;; Fixed points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;: (fixed-point cos 1.0)

;: (fixed-point (lambda (y) (+ (sin y) (cos y)))
;:              1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


;;EXERCISE 1.37
;: (cont-frac (lambda (i) 1.0)
;:            (lambda (i) 1.0)
;:            k)


;;;SECTION 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

;: ((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


;; Newton's method

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)


(define (cube x) (* x x x))

;: ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


;; Fixed point of transformed function

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;;EXERCISE 1.40
;: (newtons-method (cubic a b c) 1)


;;EXERCISE 1.41
;: (((double (double double)) inc) 5)


;;EXERCISE 1.42
;: ((compose square inc) 6)


;;EXERCISE 1.43
;: ((repeated square 2) 5)


(display "\nRESULTS\n")
(results)
