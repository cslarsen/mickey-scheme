#|
   libffi interface for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0

|#
(define-library (ffi libffi)
  (import (scheme base)
          (mickey library))

  (export
    call-foreign-function
    prepare-call-interface)

  (begin
    (open-internal-library "libffi.so" 'lazy 'global)
    (define prepare-call-interface (bind-procedure "proc_ffi_prep_cif"))
    (define call-foreign-function (bind-procedure "proc_ffi_call"))))
