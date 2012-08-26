; Hello world
(import (scheme base))
(import (scheme write))

(display (string-append "Hello" ", " ; comment at end of line
                        "world!\n"
  ; will not be printed "<secret>"
                        "-- Mickey scheme\n\n"))
