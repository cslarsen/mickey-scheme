(import (scheme base)
        (scheme write))
(define we-dont-want-to-print #f)
(display
  (unless we-dont-want-to-print "HEY\n"))
