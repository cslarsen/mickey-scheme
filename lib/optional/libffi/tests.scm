(import (scheme-experimental unit-testing)
        (scheme base)
        (prefix (ffi libffi) ffi-))

(unit-tests "(ffi libffi)"
 (assert (= 1 1)))
