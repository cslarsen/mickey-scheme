(import (scheme base))
(import (scheme lazy))
(import (scheme write))

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                  count
                  (force p)))))
(define x 5)

(display "force test: expect <promise>: ")
(display p)
(newline)

(display "force test: expect 6: ")
(display (force p))
(newline)

(display "force test: expect <promise>: ")
(display p)
(newline)

(begin (set! x 10)
       (display "force test: expect 6, because x should point to another: ")
       (display (force p))
       (newline))
