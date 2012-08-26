(import
  (prefix (scheme base) base:)
  (scheme write))

;
; The below base:unless is implemented in source
; code using 'not', but 'not' is above us
; prefixed with base:, so unless can't see it.
;
; When writing unless, one shold not have to
; concern ourselves with this kind of stuff.
; It should just work.
;
(define dont-wanna #f)
(base:unless dont-wanna (display "Hoo\n"))
