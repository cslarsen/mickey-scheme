#|
   Should output the following:

6 is even
0 is 
<Unknown: 0>
unknown!
7 is odd

|#
(import (scheme base))
(import (scheme write))

(define (unknown x)
  (newline)
  (display "<Unknown: ")
  (display x)
  (display ">")
  (newline)
  "unknown!")

(define (test-case num)
  (display num)
  (display " is ")
  (display
    (case num
      ((1 3 5 7 9) 'odd)
      ((2 4 6 8 10) 'even)
      ((100) 'one 'hundred 'bucks) ; returns 'bucks
      ((55) => display) ; returns unspecified
      (else => unknown)))
  (newline))

(test-case (* 2 3))
(test-case 0)
(test-case 7)
