(import (scheme cxr)
        (test unit-test)
        (only (scheme write) display))

(define name "(scheme cxr)")

;; car and cdr
(testq name (caddr '(1 2 3 4)) 3)
(test name (cdddr '(1 2 3 4 5)) '(4 5))
(testq name (cadddr '(1 2 3 4 5)) 4)
