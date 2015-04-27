;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define-library (implementation base)
  (import (mickey library)
          (scheme base))

  (export
    implementation-name
    implementation-version
    size-of)

  (begin
    (open-internal-library-determine-extension
      "libimplementation-base" 'global 'lazy)

    (define implementation-version
      (bind-procedure "proc_implementation_version"))

    (define implementation-name
      (bind-procedure "proc_implementation_name"))

    (define size-of
      (bind-procedure "proc_sizeof"))))
