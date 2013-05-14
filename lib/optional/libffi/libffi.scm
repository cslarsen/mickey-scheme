#|
   libffi interface for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0

|#
(define-library (ffi libffi)
  (import (scheme base)
          (mickey library))

  (export
    call-function
    make-interface
    make-type
    size-of
    value->character
    value->float
    value->integer
    value->pointer
    value->string
    value->vector)

  (begin
    (open-internal-library "libffi.so" 'lazy 'global)

    (define call-function     (bind-procedure "proc_ffi_call"))
    (define make-interface    (bind-procedure "proc_ffi_prep_cif"))
    (define make-type         (bind-procedure "proc_make_type"))
    (define size-of           (bind-procedure "proc_size_of"))
    (define value->bytevector (bind-procedure "proc_retval_to_u8vector"))
    (define value->character  (bind-procedure "proc_retval_to_uchar"))
    (define value->double     (bind-procedure "proc_retval_to_double"))
    (define value->float      (bind-procedure "proc_retval_to_float"))
    (define value->integer    (bind-procedure "proc_retval_to_integer"))
    (define value->pointer    (bind-procedure "proc_retval_to_pointer"))
    (define value->string     (bind-procedure "proc_retval_to_string"))))
