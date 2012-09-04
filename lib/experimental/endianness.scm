#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (mickey endianness)
  (import (scheme base))

  (export
    get-endianness
    is-little-or-big?)

  (cond-expand
    ((or little-endian big-endian) (define (is-little-or-big?) #t))
    (else                          (define (is-little-or-big?) #f)))

  (cond-expand
    (little-endian (define (get-endianness) 'little-endian))
    (big-endian    (define (get-endianness) 'big-endian))
    (else          (define (get-endianness) 'unknown-endian))))
