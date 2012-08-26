;; r7rs, chapter 6.4
(import (scheme base))
(import (scheme write))

(define-syntax run
  (syntax-rules ()
    ((run code)
       (begin
         (display (quote code))
         (display " => ")
         (display code)
         (newline)))))

(run (map cadr '((a b) (d e) (g h)))) ; ==> (b e h)
(run (map (lambda (n) (expt n n)) '(1 2 3 4 5))) ; ==> (1 4 27 256 3125)

(run (let ((a 2))
          (map (lambda (n)
                 (expt n a))
               '(1 2 3 4 5)))) ; ==> (1 4 9 16 25)

(run (map + '(1 2 3) '(4 5 6))) ; ==> (5 7 9)

(run (let ((count 0))
  (map (lambda (ignored)
        (set! count (+ count 1))
        count)
        '(a b)))) ; ==> (1 2) __or__ (2 1)
