(import (scheme base))
(import (scheme write))

(define-syntax hello
  (syntax-rules ()
    ((_ foo . bar)
     (begin
       (display "foo=")
       (display (quote foo))
       (display " bar=")
       (display (quote bar))
       (newline)))))

(hello world) ; ==> foo=world bar=()
(hello there brave new world!) ; ==> foo=there bar=(brave new world!)
