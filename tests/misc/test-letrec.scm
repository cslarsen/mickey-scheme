(import (scheme base))
(import (scheme write))

; Test letrec
(letrec ((my-odd? (lambda (x)
                 (if (= x 0) #f
                     (my-even? (- x 1)))))

        (my-even? (lambda (x)
                    (if (= x 0) #t
                      (my-odd? (- x 1)))))
  
        (yesno (lambda (x)
                 (if x "yes." "no. ")))
  
        (test (lambda (x)
                (if (< x 0)
                    (begin
                      (display "Too small number ")
                      (display x)
                      (newline))
                    (begin
                      (display "Number ")
                      (display x)
                      (display " even? ")
                      (display (yesno (my-even? x)))
                      (display " odd? ")
                      (display (yesno (my-odd? x)))
                      (newline))))))
        (test 0)
        (test 1)
        (test 2)
        (test 3)
        (test 4)
        (test 5))
