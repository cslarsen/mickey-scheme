(import (scheme base))
(import (scheme write))

(display "Should get (2 1) now: ")

(display
(let ((x 1) (y 2))
  (define-syntax swap!
    (syntax-rules ()
      ((swap! a b)
       (let ((tmp a))
         (set! a b)
         (set! b tmp)))))
  (swap! x y)
  (list x y)))

(newline)
