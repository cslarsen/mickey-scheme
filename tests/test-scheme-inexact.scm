(import (scheme inexact)
        (test unit-test)
        (only (scheme write) display))

(define name "(scheme inexact)")

(testq name (ceiling 3.0) 3.0)
(testq name (ceiling 3.1) 4.0)
(testq name (ceiling 3.4) 4.0)
(testq name (ceiling 3.5) 4.0)
(testq name (ceiling 3.6) 4.0)

(testq name (floor 3.0) 3.0)
(testq name (floor 3.1) 3.0)
(testq name (floor 3.4) 3.0)
(testq name (floor 3.5) 3.0)
(testq name (floor 3.6) 3.0)
(testq name (floor 3.9) 3.0)
(testq name (floor 3.999) 3.0)

(test name (sqrt 3.999) 1.99975)
(testq name (sqrt 4.0) 2.0)
(test name (sqrt -4.0) (sqrt -1)) ; nan

(test name (exp 1) 2.71828)
(test name (exp 2) 7.38906)

(test name (atan 45.0) 1.54858)
(test name (atan 12.3) 1.48967)

(test name (acos 1.0) 0.0)
(test name (acos 0.5) 1.0472)

(test name (asin 1.0) 1.5708)
(test name (asin 0.5) 0.523599)

(test name (tan 0.5) 0.546302)
(test name (cos 0.5) 0.877583)
(test name (sin (* 0.5 3.1415926535)) 1.0)

(test name (log 256) 5.54518)
(test name (/ (log 256) (log 2)) 8.0)
(test name (/ (log 1024) (log 2)) 10.0)

(test name (log 0) -inf.0)
(test name (- (log 0)) +inf.0)
