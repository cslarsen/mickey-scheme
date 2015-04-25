;; Tabulation of Hofstadter's male and female sequences, see
;; https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences

(import (scheme base)
        (scheme write)
        (test unit-test))

(letrec ((female (lambda(n)
                   (if (= n 0) 1
                       (- n (male (female (- n 1)))))))
         (male (lambda(n)
                 (if (= n 0) 0
                     (- n (female (male (- n 1)))))))

         (seq (lambda (n)
                (list (male n) (female n))))

         (name "hofstadter-seq"))

  (test name (seq 0) '(0 1))
  (test name (seq 1) '(0 1))
  (test name (seq 2) '(1 2))
  (test name (seq 3) '(2 2))
  (test name (seq 4) '(2 3))
  (test name (seq 5) '(3 3))
  (test name (seq 6) '(4 4))
  (test name (seq 7) '(4 5))
  (test name (seq 8) '(5 5))
  (test name (seq 9) '(6 6))
  (test name (seq 10) '(6 6))
  (test name (seq 11) '(7 7))
  (test name (seq 12) '(7 8))
  (test name (seq 13) '(8 8))
  (test name (seq 14) '(9 9))
  (test name (seq 15) '(9 9))
  (test name (seq 16) '(10 10))
  (test name (seq 17) '(11 11))
  (test name (seq 18) '(11 11))
  (test name (seq 19) '(12 12))
  (test name (seq 20) '(12 13)))
