#|

   Simple R7RS sequence library

   Copyright (C) 2013 Christian Stigen Larsen

   Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
   See the file LICENSE for full text.

   TODO:

     - Add lazy sequences
     - Add sequence generators
     - Add open-ended sequences (for lazy streams and generators)
     - Add third parameter to sequence w/increment, e.g.
         (seq 10 15 2) ==> (10 12 14)

|#
(define-library (portable sequence)
  (import (scheme base)
          (scheme case-lambda))
  (export seq)
  (begin
    ;; Return list of numbers in integer interval [start stop].
    ;;
    ;; Examples:
    ;
    ;;   (seq 3 7) ==> (3 4 5 6 7)
    ;;   (seq 7 3) ==> (7 6 5 4 3)
    ;;   (seq 7) ==> (7)
    ;;   (seq 7 7) ==> (7)
    ;;   (seq 7 8) ==> (7 8)
    ;;
    (define seq
      (case-lambda
        ((start) (seq start start))
        ((start stop)
           (let
             ((out '())
              (inc (if (>= stop start) 1 -1)))

             (let loop
               ((cur start))
               (set! out (append out (list cur)))
               (if (not (= cur stop))
                   (loop (+ inc cur))))

             out))))))

;; Test-cases, requires the (println) function which
;; calls (display) on each argument and adds a newline.
;;
;(for-each
;  (lambda (test)
;    (let* ((code (car test))
;           (want (cadr test))
;           (resu (eval code))
;           (good (equal? want resu)))
;
;          (println
;            (if good " OK " "FAIL") " "
;            code " ==> " resu
;            (if (not good) " != ")
;            (if (not good) want))))
;
;  '(((seq 3 7) (3 4 5 6 7))
;    ((seq 7 3) (7 6 5 4 3))
;    ((seq 7) (7))
;    ((seq 7 7) (7))
;    ((seq 7 8) (7 8))
;    ((seq 8 7) (8 7))
;    ((seq 1 3) (1 2 3))
;    ((seq 3 1) (3 2 1))
;    ((seq -3 3) (-3 -2 -1 0 1 2 3))
;    ((seq 3 -3) (3 2 1 0 -1 -2 -3))))

