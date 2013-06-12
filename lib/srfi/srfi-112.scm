;; Implementation of SRFI-112
;; http://srfi.schemers.org/srfi-112/srfi-112.html
;;
;; NOTE: SRFI-112 is currently in draft status, and
;;       could change.
;;
;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

(define-library (srfi 112)
  (import (scheme base)
          (unix uname)
          (implementation base))

  (export
    c-memory-model
    cpu-architecture
    implementation-name
    implementation-version
    os-type
    os-version
    system-instance)

  (begin

    ;; Returns value for key in a-list, or #f if not found
    ;;
    (define (lookup key alist)
      (let
        ((value (assq key alist)))

        (if value
          (cadr value)
          #f)))

    (define (cpu-architecture)
      (lookup 'machine (uname)))

    (define (c-memory-model)
      ;; Table from http://www.unix.org/whitepapers/64bit.html
      #f)

    (define (system-instance)
      (lookup 'nodename (uname)))

    (define (os-type)
      (lookup 'sysname (uname)))

    (define (os-version)
      (string-append
        (lookup 'version (uname))
        " "
        (lookup 'release (uname))))))
