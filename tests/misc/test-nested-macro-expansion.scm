;;
;; Test double macro invocation.
;; -----------------------------
(import (scheme base))
(import (scheme write))

;; Normal macro
;;
(define-syntax macro0
  (syntax-rules ()
    ((macro0 a b)
     (let
       ((ax (list (quote "a: ") a))
        (bx (list (quote "b: ") b)))
       (display "macro0: ")
       (display ax)
       (display bx)
       (newline)))))

;; Macro that calls another macro
;;
(define-syntax macro1
  (syntax-rules ()
    ((macro1 a b)
     (let
       ((ax (list (quote "mac1a: ") a))
        (bx (list (quote "mac1b: ") b)))
       (display "macro1: ")
       (macro0 ax bx)))))

(macro0 "abba" "babba") ; ==> macro0: (a:  abba)(b:  babba)
(macro1 "abba" "babba") ; ==> macro1: macro0: (a:  (mac1a:  abba))(b:  (mac1b:  babba))
