(import (scheme-experimental unit-testing)
        (scheme base)
        (prefix (crypto gnupg) gpg-))

(unit-tests "(crypto gnupg)"
 (assert (> (string-length (gpg-version)) 0))
 (assert-eqv (gpg-version) "1.3.2"))
