(import (prefix (crypto gnupg) gpg-)
        (scheme base)
        (scheme write)
        (portable print))

(println "Using GPGME version " (gpg-version) "\n")

(for-each
  (lambda (key)
    (printsln "Name:   " (cadr (assv 'name key)))
    (printsln "Email:  " (cadr (assv 'email key)))
    (printsln "KeyID:  " (cadr (assv 'keyid key)))
    (printsln "Secret: " (if (cadr (assv 'secret key))
                             "Yes" "No"))
    (println))
  (gpg-public-keys))
