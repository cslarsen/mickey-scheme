(import 
        (prefix
          (rename
            (scheme base)
            (unless my-unless))
          base:)
        (rename (scheme write)
                (display print-this)))

(define dontwanna #f)

(print-this "And ALSO this ==> ")
(print-this (base:my-unless dontwanna "YEAH!\n"))
