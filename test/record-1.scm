;; Test of the record-type system

(import (scheme base)
        (portable print))

;; Example record taken from R7RS.
;;
(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(define obj1 (kons 11 22))
(define obj2 (kons 33 44))

(println "obj1\n"
         "  kar: " (kar obj1) " -- should be 11\n"
         "  kdr: " (kdr obj1) " -- should be 22\n"
         "obj2\n"
         "  kar: " (kar obj2) " -- should be 33\n"
         "  kdr: " (kdr obj2) " -- should be 44\n")

(println "... setting kar of obj1 to 999\n")

(set-kar! obj1 999)

(println "obj1\n"
         "  kar: " (kar obj1) " -- should be 999\n"
         "  kdr: " (kdr obj1) " -- should be 22\n"
         "obj2\n"
         "  kar: " (kar obj2) " -- should be 33\n"
         "  kdr: " (kdr obj2) " -- should be 44\n")

(println
  "(pare? obj1) ==> " (pare? obj1) "\n"
  "(pare? obj2) ==> " (pare? obj2) "\n"
  "(pare? (list 1 2 3)) ==> " (pare? (list 1 2 3)))
