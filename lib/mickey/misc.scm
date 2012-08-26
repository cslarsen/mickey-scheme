#|

Library: (mickey misc)

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(define-library (mickey misc)
  (import (only (scheme base) define)
          (mickey library))

  (export
    :backtrace
    :circular?
    :closure-source
    :debug
    :list->dot
    :llvm:gcd
    :syntax-expand
    :type-of
    :version)

  (begin
    (open-internal-library "libmickey-misc.so")

    (define :backtrace      (bind-procedure "proc_backtrace"))
    (define :circular?      (bind-procedure "proc_circularp"))
    (define :closure-source (bind-procedure "proc_closure_source"))
    (define :debug          (bind-procedure "proc_debug"))
    (define :list->dot      (bind-procedure "proc_list_to_dot"))
    (define :llvm:gcd       (bind-procedure "proc_llvm_gcd"))
    (define :syntax-expand  (bind-procedure "proc_syntax_expand"))
    (define :type-of        (bind-procedure "proc_type_of"))
    (define :version        (bind-procedure "proc_version"))))
