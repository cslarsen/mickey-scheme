;; Need to keep track of bugs somewhere, and it's better to do that right in
;; the testing framework.

(import (scheme char)
        (test unit-test)
        (only (scheme write) display))

(begin
  ;; Syntax-rules bugs. Difference in these two seems to be the environments.

  (test
    (begin
      (define-syntax increment
        (syntax-rules ()
          ((_ x) (begin (set! x (+ x 1)) x)) ;; increment by one
          ((_ x i) (begin (set! x (+ x i)) x)))) ;;  increment by i

      (let ((i -10)
            (j -20))
        (list (increment i)
              (increment j 3))))
    (list -9 -17))

  (xfailq
    (begin
      (define-syntax increment
        (syntax-rules ()
          ((_ x) (begin (set! x (+ x 1)) x)) ;; increment by one
          ((_ x i) (begin (set! x (+ x i)) x)))) ;;  increment by i

      (let ((i -10)
            (j -20))
        (increment i)
        (increment j 3)
        (list i j)))
    (list -9 -17)
    "Known syntax-rules bug"))

(tap-results)
