(import (scheme write)
        (scheme base)
        (mickey misc)
        (test unit-test))

(testq (:type-of (begin)) 'nil)
(testq (begin 1) 1)
(testq (begin 1 2) 2)
(testq (begin 1 2 3) 3)

(let
  ((a #f)
   (b #f)
   (c #f))
  (testq
    (begin
      (set! a 111)
      (testq a 111)
      (testq b #f)
      (testq c #f)
      (set! b 222)
      (testq a 111)
      (testq b 222)
      (testq c #f)
      (set! c 333)
      (testq a 111)
      (testq b 222)
      (testq c 333) 444) 444))

(testq
 (let
   ((s ""))
   (begin
    (set! s (string-append s "a"))
    (set! s (string-append s "b"))
    (set! s (string-append s "c")))
   s) "abc")

(tap-results)
