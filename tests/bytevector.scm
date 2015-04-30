;; Bytevector tests

(import (scheme write)
        (scheme base)
        (test unit-test))

(test (bytevector? (vector 1 2 3)) #f)
(test (bytevector? (make-bytevector 3)) #t)
(test (bytevector? (make-bytevector 3 11)) #t)
(test (make-bytevector 3) #u8(0 0 0))
(test (make-bytevector 3 11) #u8(11 11 11))

(test
  (let ((v1 (make-bytevector 3)))
    (bytevector-u8-set! v1 0 11)
    v1)
  #u8(11 0 0))

(test
  (let ((v1 (make-bytevector 3)))
    (bytevector-u8-set! v1 0 1)
    (bytevector-u8-set! v1 1 22)
    (bytevector-u8-set! v1 2 33)
    (bytevector-u8-set! v1 0 11)
    v1)
  #u8(11 22 33))

(test
  (let ((v1 (make-bytevector 3 111))
        (v2 (make-bytevector 3 99)))
    (bytevector-u8-set! v1 1 222)
    (bytevector-copy! v1 v2)
    v2)
  #u8(111 222 111))

(test (bytevector? #u8(1 2 3)) #t)
(test (bytevector? (vector 1 2 3)) #f)

(test (bytevector-u8-ref #u8(11 22 33) 0) 11)
(test (bytevector-u8-ref #u8(11 22 33) 1) 22)
(test (bytevector-u8-ref #u8(11 22 33) 2) 33)
(test (bytevector-length #u8()) 0)
(test (bytevector-length #u8(1)) 1)
(test (bytevector-length #u8(1 1)) 2)
(test (bytevector-length #u8(1 1 1)) 3)
(test (bytevector-length (make-bytevector 1000)) 1000)
(test (bytevector? #u8()) #t)

(test
  (let* ((v1 #u8(11 22 33 44 55 66 77 88 99))
         (v2 (make-bytevector (bytevector-length v1))))
   (bytevector-copy-partial! v1 0 2 v2 3)
   v2)
  #u8(0 0 0 11 22 0 0 0 0))

(test
  (let* ((v1 #u8(11 22 33 44 55 66 77 88 99))
         (v2 (make-bytevector (bytevector-length v1))))
   (bytevector-copy-partial! v1 2 6 v2 2)
   v2)
  #u8(0 0 33 44 55 66 0 0 0))

(tap-results)
