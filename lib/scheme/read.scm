#|

Mickey R7RS Scheme

Copyright (C) 2012-2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#
(define-library (scheme read)
  (import (scheme base)
          (scheme case-lambda)
          (mickey library))

  (export read)

  (begin
    (open-internal-library-determine-extension "libscheme-read" 'lazy 'global)
    (define read-port (bind-procedure "proc_read_from_port"))

    ;; (read) --> reads from current-input-port
    ;; (read <port>) --> reads from given port
    ;;
    ;; Returns a list parsed from the input.
    ;;
    (define read
      (case-lambda
        (() (read-port (current-input-port)))
        ((port) (read-port port))))))
