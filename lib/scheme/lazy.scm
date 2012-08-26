#|

Mickey Scheme R7RS lazy evaluation library

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                          \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme lazy)
  (import (scheme base))
  (export delay force)
  (begin
    #|
       Lazy evaluation with memoization.
    |#
    (define-syntax delay
      (syntax-rules ()
        ((delay expression)
         (let
           ((computed? #f)
            (memoized '()))
           (lambda ()
             (if computed? memoized
               (begin
                 (set! memoized expression)
                 (set! computed? #t)
                 memoized)))))))

    #|
       Force a previously delayed computation.

       After forcing the first time, the computer value is memoized, and
       thus not recomputed every time.
    |#
    (define (force promise)
      (promise))))
