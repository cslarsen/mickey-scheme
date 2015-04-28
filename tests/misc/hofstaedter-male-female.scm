;; Tabulation of Hofstadter's male and female sequences
(import (scheme base))
(import (scheme write))

(letrec ((female (lambda(n)
                   (if (= n 0) 1
                       (- n (male (female (- n 1)))))))
         (male (lambda(n)
                 (if (= n 0) 0
                     (- n (female (male (- n 1))))))))
  (display "i male(i) female(i)")(newline)
  (do ((i 0 (+ i 1)))
      ((> i 8) #f)
    (display i) (display "   ")(display (male i))(display "         ")(display (female i))
    (newline)))
