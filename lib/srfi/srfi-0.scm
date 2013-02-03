#|
   SRFI-0 for Mickey Scheme
   http://srfi.schemers.org/srfi-0/srfi-0.html

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the GNU LGPL 2.1; see LICENSE
|#
(define-library (srfi 4)
  (import (mickey library))
  (export cond-expand)
  (begin
    (open-internal-library "libsrfi-0.so" 'lazy 'global)
    (define cond-expand (bind-syntax "srfi0_cond_expand"))))
