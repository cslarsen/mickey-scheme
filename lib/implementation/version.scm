;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define-library (implementation version)
  (import (mickey library)
          (scheme base))

  (export version name)

  (begin
    (open-internal-library "libmickey.so" 'global 'lazy)
    (define version (bind-procedure "proc_implementation_version"))
    (define name (bind-procedure "proc_implementation_name"))))
