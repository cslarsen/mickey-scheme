#|

   UNIX fork(2) for Mickey Scheme

   Copyright (C) 2013 Christian Stigen Larsen
   Distributed under the LGPL 2.1; see LICENSE

|#
(define-library (unix fork)
  (export child-process?
          fork)

  (import (only (scheme base) define)
          (mickey library))
  (begin
    (open-internal-library "libunix-fork.so" 'lazy 'global)

    ;; (fork) is a wrapper around fork(2).
    ;;
    ;; It creates a copy of the current process, returning
    ;;
    ;;   * 0 to the child process
    ;;   * the child's PID to the parent process
    ;;
    (define fork
      (bind-procedure "proc_fork"))

    ;; Returns #t if PID from (fork) is zero, meaning it is a child process.
    ;;
    ;; This function is merely added for code readability (i.e., in C you
    ;; would never do this, but in Scheme I think it's both fitting and good
    ;; practice).
    ;;
    (define (child-process? pid)
      (and (number? pid) (= pid 0)))))
