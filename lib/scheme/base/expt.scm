;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; Slow O(n), implement O(log n) instead.
;;
(define (expt base exponent)
  (define (square n)
    (* n n))

  (cond
    ((not (or (number? base)
              (number? exponent)))
     (error "Not a number"))

    ((and (integer? exponent)
          (>= exponent 0))
     (let loop
       ((a base)
        (n exponent))
       (cond
         ((and (zero? a)
               (zero? n)) 1)
         ((= n 0) 1)
         ((= n 1) a)
         (else (loop (* a base) (- n 1))))))

    ((negative? exponent)
     (expt (/ 1 base) (abs exponent))) ; a^(-n) = (1/a)^n

    (else
      (error "Only integer exponents are supported currently"))))
