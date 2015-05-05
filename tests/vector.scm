(import (scheme base)
        (scheme write)
        (test unit-test))

(test (vector 1 2 3 (list 4 5) 6) #(1 2 3 (4 5) 6))
(testq (vector? (vector 'abba)) #t)
(testq (vector-length (vector 1 2 3 (list 4 5) 6)) 5)
(test (make-vector 7 'a) (vector 'a 'a 'a 'a 'a 'a 'a))
(test (make-vector 7 'a) #(a a a a a a a))

;; Here are some known bugs
(test (make-vector 1) (vector (vector-ref (make-vector 1) 0)))
(test (vector (+ 1 1) (* 2 2)) (vector 2 4))
(test #((+ 1 1)) (vector '(+ 1 1)))

(testq (vector-ref (vector 1 2 3 (list 4 5) 6) 0) 1)
(testq (vector-ref (vector 1 2 3 (list 4 5) 6) 1) 2)
(testq (vector-ref (vector 1 2 3 (list 4 5) 6) 2) 3)
(test (vector-ref (vector 1 2 3 (list 4 5) 6) 3) (list 4 5)) ; known bug
(testq (vector-ref (vector 1 2 3 (list 4 5) 6) 4) 6)

(test
  (let ((v (vector 0 1 2)))
    (vector-set! v 0 10)
    v)
  #(10 1 2))

(test (vector->list (vector 1 2 3)) (list 1 2 3))
(test (vector->list #(1 2 3)) (list 1 2 3))
(test (vector->list (vector 1 (+ 1 1) 3)) (list 1 2 3))

(test (list->vector '(1 2 3)) #(1 2 3))
(test (list->vector '(1 2 (3 4))) #(1 2 (3 4)))
(test (list->vector '(1 2 (3 4))) (vector 1 2 '(3 4)))
(test (list->vector '(1 2 (3 4))) (vector 1 2 (quote (3 4))))

(test (vector->string (vector #\a #\b #\b #\a)) "abba")
(test (vector->string (vector #\a #\b #\c #\d)) "abcd")

(test (string->vector "Hello!") #(#\H #\e #\l #\l #\o #\!))

(test
  (let* ((v #(1 2 3))
         (w (vector-copy v)))
   (vector-set! v 0 10)
   (list v w))
  (list #(10 2 3) #(1 2 3)))

(testq (vector-length #()) 0)
(testq (vector-length #(1)) 1)
(testq (vector-length #(1 2 3)) 3)
(test
  (let ((v #(1 2 3)))
    (vector-fill! v #\x)
    v) #(#\x #\x #\x))

(tap-results)
