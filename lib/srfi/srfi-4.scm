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

    s8vector? u8vector?
    s16vector? u16vector?
    s32vector? u32vector?
    s64vector? u64vector?

    s8vector u8vector
    s16vector u16vector
    s32vector u32vector
    s64vector u64vector

    s8vector-length u8vector-length
    s16vector-length u16vector-length
    s32vector-length u32vector-length
    s64vector-length u64vector-length

    s8vector-ref u8vector-ref
    s16vector-ref u16vector-ref
    s32vector-ref u32vector-ref
    s64vector-ref u64vector-ref

    s8vector-set! u8vector-set!
    s16vector-set! u16vector-set!
    s32vector-set! u32vector-set!
    s64vector-set! u64vector-set!

    list->s8vector list->u8vector
    list->s16vector list->u16vector
    list->s32vector list->u32vector
    list->s64vector list->u64vector

    s8vector->list u8vector->list
    s16vector->list u16vector->list
    s32vector->list u32vector->list
    s64vector->list u64vector->list)
  (begin
    (open-internal-library-determine-extension "libsrfi-4" 'lazy 'global)

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
    (define u64vector? (bind-procedure "u64vectorp"))

    (define s8vector (bind-procedure "s8vector"))
    (define u8vector (bind-procedure "u8vector"))
    (define s16vector (bind-procedure "s16vector"))
    (define u16vector (bind-procedure "u16vector"))
    (define s32vector (bind-procedure "s32vector"))
    (define u32vector (bind-procedure "u32vector"))
    (define s64vector (bind-procedure "s64vector"))
    (define u64vector (bind-procedure "u64vector"))

    (define s8vector-length (bind-procedure "s8vector_length"))
    (define u8vector-length (bind-procedure "u8vector_length"))
    (define s16vector-length (bind-procedure "s16vector_length"))
    (define u16vector-length (bind-procedure "u16vector_length"))
    (define s32vector-length (bind-procedure "s32vector_length"))
    (define u32vector-length (bind-procedure "u32vector_length"))
    (define s64vector-length (bind-procedure "s64vector_length"))
    (define u64vector-length (bind-procedure "u64vector_length"))

    (define s8vector-ref (bind-procedure "s8vector_ref"))
    (define u8vector-ref (bind-procedure "u8vector_ref"))
    (define s16vector-ref (bind-procedure "s16vector_ref"))
    (define u16vector-ref (bind-procedure "u16vector_ref"))
    (define s32vector-ref (bind-procedure "s32vector_ref"))
    (define u32vector-ref (bind-procedure "u32vector_ref"))
    (define s64vector-ref (bind-procedure "s64vector_ref"))
    (define u64vector-ref (bind-procedure "u64vector_ref"))

    (define s8vector-set! (bind-procedure "s8vector_set"))
    (define u8vector-set! (bind-procedure "u8vector_set"))
    (define s16vector-set! (bind-procedure "s16vector_set"))
    (define u16vector-set! (bind-procedure "u16vector_set"))
    (define s32vector-set! (bind-procedure "s32vector_set"))
    (define u32vector-set! (bind-procedure "u32vector_set"))
    (define s64vector-set! (bind-procedure "s64vector_set"))
    (define u64vector-set! (bind-procedure "u64vector_set"))

    (define list->s8vector (bind-procedure "list_to_s8vector"))
    (define list->u8vector (bind-procedure "list_to_u8vector"))
    (define list->s16vector (bind-procedure "list_to_s16vector"))
    (define list->u16vector (bind-procedure "list_to_u16vector"))
    (define list->s32vector (bind-procedure "list_to_s32vector"))
    (define list->u32vector (bind-procedure "list_to_u32vector"))
    (define list->s64vector (bind-procedure "list_to_s64vector"))
    (define list->u64vector (bind-procedure "list_to_u64vector"))

    (define s8vector->list (bind-procedure "s8vector_to_list"))
    (define u8vector->list (bind-procedure "u8vector_to_list"))
    (define s16vector->list (bind-procedure "s16vector_to_list"))
    (define u16vector->list (bind-procedure "u16vector_to_list"))
    (define s32vector->list (bind-procedure "s32vector_to_list"))
    (define u32vector->list (bind-procedure "u32vector_to_list"))
    (define s64vector->list (bind-procedure "s64vector_to_list"))
    (define u64vector->list (bind-procedure "u64vector_to_list"))))
