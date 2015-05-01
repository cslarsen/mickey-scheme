(import (scheme write))
(import (scheme base))

;; TODO: It would be much better to use string ports and parameterize, e.g.
;;
;; (let
;;   ((actual (open-output-string))
;;    (expected "..."))
;;
;;   (parameterize
;;     ((current-output-port actual))
;;     (display "..."))
;;
;;   (testq (get-output-string actual) expected))

;; Block comments
(display "(1) Testing block comments\n")

#|
 (display "This line should NOT be shown (1)\n")
|#

(display "(2) Testing nested comments\n")

;; Nested block comments
#|
  (display "This line should NOT be shown (2)\n")
  #| (display "This line should NOT be shown (3)\n") |#
  (display "This line should NOT be shown (4)\n")
|#

(display "(3) Should get 2 here: ")
(display (+  #| this is a #| nested |# block comment |# 2))
(display "\n")

(display "(4) End of comment-tests\n")
