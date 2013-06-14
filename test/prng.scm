(import (scheme base)
        (scheme time)
        (portable print)
        (portable prng lcg))

(define prng (lcg-glibc (current-second)))

(println "Some random numbers:")

(do ((i 0 (+ i 1)))
    ((> i 20) (println))
    (print (prng) " "))
