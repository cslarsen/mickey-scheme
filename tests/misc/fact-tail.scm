(import (scheme base))
(import (scheme write))

;; Tail call optimized factorial,
;; i.e., suitable for elimination
;; (conversion to iterative execution)

(define (fact n)

  ; Helper-function that accumulates
  ; answer in parameters to function
  (define (fact-acc n acc)
    (if (zero? n) acc
        ; Recursive tail call
        (fact-acc (- n 1) (* n acc))))

  ; Start the ball rolling!
  (fact-acc n 1))

(define (test-fact n)
  (display (string-append
    (number->string n) "! = " (number->string (fact n)) "\n")))

;; All of these should return,
;; even if the answer is overflowed (i.e., "+Inf").

(test-fact 0)
(test-fact 1)
(test-fact 2)
(test-fact 4)
(test-fact 8)
(test-fact 16)
(test-fact 32)
(test-fact 64)
(test-fact 128)
(test-fact 256)
(test-fact 512)
(test-fact 1024)
(test-fact 2048)
(test-fact 4096)
(test-fact 8192)
(test-fact 32768)
