(import (scheme-experimental documentation))

(define-library-documentation (crypto gnupg)
  "Bindings for GnuPG Made Easy (GPGME)"

  (procedure version "Returns version number")
  (procedure public-keys "Returns public keys"
    (usage
      ((public-keys) "List all keys")
      ((public-keys pattern) "Only return keys with given pattern")
      ((public_keys pattern secret-only?)
         "Only return secret keys with given pattern"))))
