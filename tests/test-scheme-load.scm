(import
 (scheme load)
 (scheme base)
 (only (scheme write) display)
 (test unit-test))
(begin
  (load "complex.scm")
  (testq
    (complex->string
      (*complex (make-complex 2 3)
                (make-complex 5 7)))
    "-11 + 29i"))

(tap-results)
