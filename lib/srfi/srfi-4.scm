#|
   SRFI-4 for Mickey Scheme
   http://srfi.schemers.org/srfi-4/srfi-4.html

   Copyright (C) 2012 Christian Stigen Larsen
   Distributed under the GNU LGPL 2.1; see LICENSE
|#
(define-library (srfi 4)
  (import (mickey library))
  (export
    make-s8vector make-u8vector
    make-s16vector make-u16vector
    make-s32vector make-u32vector
    make-s64vector make-u64vector

    s8vector?  u8vector?
    s16vector?  u16vector?
    s32vector?  u32vector?
    s64vector?  u64vector?)

  (begin
    (open-internal-library "libsrfi-4.so")

    (define make-s8vector (bind-procedure "make_s8vector"))
    (define make-u8vector (bind-procedure "make_u8vector"))
    (define make-s16vector (bind-procedure "make_s16vector"))
    (define make-u16vector (bind-procedure "make_u16vector"))
    (define make-s32vector (bind-procedure "make_s32vector"))
    (define make-u32vector (bind-procedure "make_u32vector"))
    (define make-s64vector (bind-procedure "make_s64vector"))
    (define make-u64vector (bind-procedure "make_u64vector"))

    (define s8vector? (bind-procedure "s8vectorp"))
    (define u8vector? (bind-procedure "u8vectorp"))
    (define s16vector? (bind-procedure "s16vectorp"))
    (define u16vector? (bind-procedure "u16vectorp"))
    (define s32vector? (bind-procedure "s32vectorp"))
    (define u32vector? (bind-procedure "u32vectorp"))
    (define s64vector? (bind-procedure "s64vectorp"))
    (define u64vector? (bind-procedure "u64vectorp"))))
