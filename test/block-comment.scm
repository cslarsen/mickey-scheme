(import (scheme write))
(import (scheme base))

;; Block comments
(display "(1) Testing block comments\n")

#|
 (display "This line should NOT be shown (1)\n")
|#

(display "(2) Testing nested comments\n")

#|
  (display "This line should NOT be shown (2)\n")
  #| (display "This line should NOT be shown (3)\n") |#
  (display "This line should NOT be shown (4)\n")
|#

(display "(3) Should get 2 here: ")
(display (+  #| this is a #| nested |# block comment |# 2))
(newline)

(display "(4) End of comment-tests\n")
