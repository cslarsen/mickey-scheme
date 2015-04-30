(import (scheme cxr)
        (test unit-test)
        (only (scheme write) display))

;; car and cdr
(testq (caddr '(1 2 3 4)) 3)
(test (cdddr '(1 2 3 4 5)) '(4 5))
(testq (cadddr '(1 2 3 4 5)) 4)

(tap-results)
