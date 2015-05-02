(import
  (prefix (scheme base) base-)
  (scheme write)
  (test unit-test))

; The "unless" macro is implemented using "not". But "not" is in (scheme base),
; so we'll add a prefix to all (scheme base) definitions and make sure it still
; works.

(testq
  (base-let*
    ((something-happen #f)
     (value 0))
   (base-unless something-happen (base-set! value 111))
   value) 111)

(tap-results)
