(import (scheme base)
        (scheme write))

(display
  (cond-expand
    (mickey "You are running Mickey Scheme.")
    (chibi  "You are running Chibi Scheme.")
    (else   "You are running an unknown Scheme system.")))
(newline)

(display
  (cond-expand
    (unix  "You are on a UNIX system.")
    (win32 "You are on a WINDOWS sytem.")
    (else   "You are on an unknown operating system.")))
(newline)
