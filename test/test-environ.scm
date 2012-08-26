(import (scheme base))
(import (scheme write))
(import (scheme process-context))

(display (string-append
  "Current username is "
  (cdr (assoc "USER" (get-environment-variables)))
  ".\n"))
