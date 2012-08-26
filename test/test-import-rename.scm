(import (rename (scheme write)
                (display print))
        (rename (scheme base)
                (newline nl)
                (caar kaar)))

(define foo '((bar baz) quux))

(print "foo always precedes ")
(print (kaar foo))
(nl)
(print "Goodbye!")
(nl)
