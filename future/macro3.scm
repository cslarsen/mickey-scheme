;; Try out some macros from
;; http://www.shido.info/lisp/scheme_syntax_e.html
;;
;; This example works fine in Chicken, but not in Mickey
(import (scheme base))
(import (scheme write))

(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(when #t (display "(when #t ...) works\n"))
(when #f (display "(when #f ...) does NOT work\n"))

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))

(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
         b1 ...
         (loop (+ 1 i)))))))

;; try out macros

(display "while: ")
(let ((i 0))
  (while (< i 10)
    (display i)
    (display " ")
    (set! i (+ i 1)))) ; ==> 0 1 2 3 4 5 6 7 8 9
; returns unspecified value
(newline)

(display "for: ")
(for (i 0 10)
  (display i)
  (display " ")) ; ==> 0 1 2 3 4 5 6 7 8 9
; returns unspecified value
(newline)
