(import (scheme base)
        (scheme write))

#|

(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

|#

(define gensym #f)
(let
  ((value -1))
  (set! gensym
    (lambda ()
      (set! value (+ 1 value))
      value)))

(define (kons x y)
  (let
    ((kons (list
             'record-type-<pare>-<gensym>
             (make-vector 2 #f)))) ; TOTAL number of fields
    ; initialized values:
    (vector-set! (cadr kons) 0 x) ; x stored at 0
    (vector-set! (cadr kons) 1 y) ; y stored at y
    kons))

(define (pare? rec)
  (and (list? rec)
       (eqv? (car rec) 'record-type-<pare>-<gensym>)
       (vector? (cadr rec))
       (= (vector-length (cadr rec)) 2))) ; TOTAL number

(define (kar record)
  (if (pare? record)
    (vector-ref (cadr record) 0) ; x stored at 0
    (error "Not a record of type <pare>")))

(define (set-kar! record value)
  (if (pare? record)
    (vector-set! (cadr record) 0 value) ; x stored at 0
    (error "Not a record of type <pare>")))

(define (kdr record)
  (if (pare? record)
    (vector-ref (cadr record) 1) ; y stored at 1
    (error "Not a record of type <pare>")))

(display (pare? (kons 1 2))) (newline) ; ==> #t
(display (pare? (cons 1 2))) (newline) ; ==> #f
(display (kar (kons 1 2))) (newline)   ; ==> 1
(display (kdr (kons 1 2))) (newline)   ; ==> 2
(display
    (let ((k (kons 1 2)))
          (set-kar! k 3)
              (kar k))) (newline) ; ==> 3
