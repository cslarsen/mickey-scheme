#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1

|#

(define-library (mickey features)
  (import (only (scheme base) define)
          (mickey library))

  (export
    features)

  (begin
    (open-self 'global)

    ;; Show currently detected features
    ;;
    (define features (bind-procedure "proc_list_features"))))
