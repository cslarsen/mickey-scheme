#|

SRFI-16 for Mickey Scheme
http://srfi.schemers.org/srfi-16/srfi-16.html

Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

 |#

(define-library (srfi 16)
  (import (mickey library))
  (export case-lambda)
  (begin
    (open-internal-library-determine-extension "libsrfi-16" 'global 'lazy)
    (define case-lambda (bind-syntax "proc_case_lambda"))))
