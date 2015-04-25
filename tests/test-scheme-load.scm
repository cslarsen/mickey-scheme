(import
 (scheme load)
 (scheme base)
 (only (scheme write) display)
 (test unit-test))

(define name "(scheme load)")

(begin
  (load "complex.scm")
  (testq name
    (complex->string
      (*complex (make-complex 2 3)
                (make-complex 5 7)))
    "-11 + 29i"))
