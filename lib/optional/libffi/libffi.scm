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
    prepare-call-interface
    return-value->integer
    return-value->pointer
    return-value->string
    size-of)

  (begin
    (open-internal-library "libffi.so" 'lazy 'global)
    (define prepare-call-interface (bind-procedure "proc_ffi_prep_cif"))
    (define call-foreign-function (bind-procedure "proc_ffi_call"))
    (define return-value->integer (bind-procedure "proc_retval_to_integer"))
    (define return-value->pointer (bind-procedure "proc_retval_to_pointer"))
    (define return-value->string (bind-procedure "proc_retval_to_string"))
    (define size-of (bind-procedure "proc_size_of"))))
