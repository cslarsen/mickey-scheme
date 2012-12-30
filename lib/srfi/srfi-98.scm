#|
   SRFI-98 for Mickey Scheme
   http://srfi.schemers.org/srfi-98/srfi-98.html
|#
(define-library (srfi 98)
  (import (scheme process-context))
  (export get-environment-variable
          get-environment-variables))
