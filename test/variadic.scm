;; Test of different variadic lambda forms
(import (scheme base))
(import (scheme write))

(define (list-of-strings->string s)
  (if (null? s) ""
    (string-append (car s)
    (if (not (zero? (length (cdr s)))) " " "")
    (list-of-strings->string (cdr s)))))

(define (->string s)
  (cond ((number? s) (number->string s))
        ((symbol? s) (symbol->string s))
        ((list? s) (list-of-strings->string s))
        ((string? s) s)
        (else "")))

(define no-args
  (lambda ()
    (display "no-args\n")))

(define one-arg
  (lambda (x)
    (display (string-append
      "one-arg, with x='" (->string x) "'\n"))))

(define one-arg-rest
  (lambda (x . rest)
    (display (string-append
      "one-arg-rest with x='" (->string x) "' and rest='" (->string rest) "'\n"))))

(define two-arg-rest
  (lambda (x y . rest)
    (display (string-append
      "two-arg-rest with x='" (->string x) "' y='" (->string y) "' and rest='" (->string rest) "'\n"))))

(define rest-arg
  (lambda rest
    (display (string-append
      "rest with rest='" (->string rest) "'\n"))))

(no-args)
(one-arg "Uno!")

(one-arg-rest "Uno" "dos!")
(one-arg-rest "Uno" "dos" "tres!")
(one-arg-rest "Uno" "dos" "tres" "cuatro!")

(two-arg-rest "Uno" "dos!")
(two-arg-rest "Uno" "dos" "tres!")
(two-arg-rest "Uno" "dos" "tres" "cuatro!")

(rest-arg "Uno!")
(rest-arg "Uno" "dos!")
(rest-arg "Uno" "dos" "tres!")
(rest-arg) ; zero

(display "Fin!\n")
