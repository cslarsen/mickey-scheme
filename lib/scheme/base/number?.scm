;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see LICENSE

;; TODO: Add complex?

(define (number? n)
  (or (integer? n)
      (real? n)
      (rational? n)))
