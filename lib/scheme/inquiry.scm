;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define-library (scheme inquiry)
  (import (srfi 112))
  (export
    c-memory-model
    cpu-architecture
    implementation-name
    implementation-version
    os-type
    os-version
    system-instance))
