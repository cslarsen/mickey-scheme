#|

   Simple R7RS gensym library

   Copyright (C) 2013 Christian Stigen Larsen

   Distributed under GNU LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
   See the file LICENSE for full text.

|#
(define-library (portable gensym)
  (import (scheme base))
  (export gensym)
  (begin
    ;; Singleton generator of process-unique symbols of form gensym-0,
    ;; gensym-1, etc.
    ;;
    (define gensym #f)

    (let
      ((value -1))
      (set! gensym
        (lambda ()
          (set! value (+ 1 value))
          (string->symbol
            (string-append
              "gensym-"
              (number->string value))))))))
