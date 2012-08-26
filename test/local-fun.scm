;; Test local function definitions
(import (scheme base))
(import (scheme write))

; Make a global inner1, which should be shadowed in
; the *real* inner defs
(define (inner1 n) 123)

(define (outer o)
  (define (inner1 n)
    (display (string-append "Inner1: " n "\n")))
  (define (inner2 n)
    (display (string-append "Inner2: " n "\n")))
  (inner1 (string-append o "Inner1"))
  (inner2 (string-append o "Inner2"))
  (inner1 (string-append o "both!"))
  (inner2 (string-append o "both1")))

(outer "Hello, ")

;; Make sure inner1 from this place refers to the real one

(if (not (eq? (inner1 456) 123))
  (display (string-append
    "Error, inner1 at global scope did not return 123, but: "
      (number->string (inner1 456) "\n")))
  (display "Ok, inner1 works fine\n"))
