#|
   GPGME (GnuPG Made Easy) bindings for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
|#
(define-library (crypto gnupg)
  (import (scheme base)
          (mickey library))
  (export
    public-keys
    version)
  (begin
    (open-internal-library "libgnupg.so")
    (define version (bind-procedure "proc_version"))
    (define public-keys (bind-procedure "proc_public_keys"))))
