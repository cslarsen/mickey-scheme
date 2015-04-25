#|
 | Correct output:
 |
 | showit2: #t
 | doh
 | showit: #t
 | Expect hello: hello
 |
 |#
(import (scheme base))
(import (scheme write))

(define (test num)
  (define (showit x)
    (display "showit: ")
    (display x)
    (newline))

  (define (showit2 x)
    (display "showit2: ")
    (display x)
    (newline))

  (cond
    ((= num 123) 'hello)
    ((< num 10) => showit)
    ((> num 10) => showit2)
    (else (display "doh\n"))))

(test 100)
(test 10)
(test 1)
(display "Expect hello: ")
(display (test 123))
(newline)
