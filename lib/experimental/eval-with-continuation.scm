(define-library (scheme-experimental eval-with-continuation)
  (import (scheme base)
          (mickey library))
  (export eval-with-continuation)
  (begin
    (open-internal-library "libeval-cont.so")
    (define eval-with-continuation (bind-procedure "proc_eval_cont"))))
