;; Uses of closure,
;; http://en.wikipedia.org/wiki/Closure_(computer_science)#Uses_of_closures
;;
;; Shows a cool way to pass private messages between two procedures
(import (scheme base))
(import (scheme write))

(define foo #f)
(define bar #f)

(let ((secret-message "none"))
  (set! foo (lambda (msg) (set! secret-message msg)))
  (set! bar (lambda () secret-message)))

(display (bar)) ; prints "none"
(newline)
(foo "meet me by the docks at midnight")
(display (bar)) ; prints "meet me by the docks at midnight"
(newline)
