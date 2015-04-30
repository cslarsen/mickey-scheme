;; Tests tail call elimination

(import (scheme base)
        (scheme write)
        (test unit-test))

(define (my-loop body count-down count-up)
   (if (> count-down 0)
    (begin
      (body)
      (my-loop body (- count-down 1) (+ count-up 1)))
    (values count-down count-up)))

(define *counter* 0)

(define (do-something)
  (set! *counter* (+ *counter* 1))
  (display ""))

;; Have my-loop recurse 60000 times, which should break the stack if tail
;; call elimination is not implemented.
(test (my-loop do-something 60000 0) (values 0 *counter*))
(tap-results)
