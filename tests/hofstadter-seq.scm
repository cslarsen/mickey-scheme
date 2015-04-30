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
                (list (male n) (female n)))))

  (test (seq 0) '(0 1))
  (test (seq 1) '(0 1))
  (test (seq 2) '(1 2))
  (test (seq 3) '(2 2))
  (test (seq 4) '(2 3))
  (test (seq 5) '(3 3))
  (test (seq 6) '(4 4))
  (test (seq 7) '(4 5))
  (test (seq 8) '(5 5))
  (test (seq 9) '(6 6))
  (test (seq 10) '(6 6))
  (test (seq 11) '(7 7))
  (test (seq 12) '(7 8))
  (test (seq 13) '(8 8))
  (test (seq 14) '(9 9))
  (test (seq 15) '(9 9))
  (test (seq 16) '(10 10))
  (test (seq 17) '(11 11))
  (test (seq 18) '(11 11))
  (test (seq 19) '(12 12))
  (test (seq 20) '(12 13))
  (tap-results))
