#|

SRFI-16 for Mickey Scheme
http://srfi.schemers.org/srfi-16/srfi-16.html

Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

 |#

(define-library (scheme math)
  (import (mickey library))
  (export case-lambda)
  (begin
    (open-internal-library "libsrfi-16.so")
    (define case-lambda (bind-syntax "proc_case_lambda"))))
