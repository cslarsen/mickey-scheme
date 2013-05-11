;; A simple, syntactically unhygienic let-implementation in R7RS Scheme.
;;
;; by Christian Stigen Larsen (public domain)
;;
;; Expected output:
;;
;; Name:    Winnie-the-Pooh
;; Likes:   (food honey)
;; Friends: (Piglet Eeyore (Christopher Robin) Rabbit)
;;
;; Fibonacci numbers less than 100: 0 1 1 2 3 5 8 13 21 34 55 89

(import (scheme base)
        (scheme write)
        (portable print))

;; Implementation of ordinary let using lambdas, named let using letrec.
;;
(define-macro %let code
  (begin
    (define (ordinary-let p)
      (define <names>  (map car (car p)))
      (define <values> (map cadr (car p)))
      (define <body>   (cdr p))

      `((lambda ,<names> ,@<body>) ,@<values>))

    (define (named-let p)
      (define <proc>   (car p))
      (define <names>  (map car (cadr p)))
      (define <values> (map cadr (cadr p)))
      (define <body>   (cddr p))

      `(letrec
         ((,<proc> (lambda ,<names> ,@<body>)))
         (,<proc> ,@<values>)))

    ((if (symbol? (car code)) named-let ordinary-let) code)))

;; EXAMPLE ORDINARY LET

(%let
  ((name    'Winnie-the-Pooh)
   (likes   '(food honey))
   (friends '(Piglet Eeyore (Christopher Robin) Rabbit)))

  (println
    "Name:    " name    "\n"
    "Likes:   " likes   "\n"
    "Friends: " friends "\n"))

;; EXAMPLE NAMED LET

(print "Fibonacci numbers less than 100: ")

(%let fibonacci
  ((a 0)
   (b 1))
  (print a " ")
  (if (< b 100)
      (fibonacci b (+ a b))))

(println)
