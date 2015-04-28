(import (scheme base)
        (scheme write))

;; The <body> in (lambda () <body>)
;; should be enclosed in implicit (begin <body>)
(define counter
  (let* ((count 0))
    (lambda ()
      (set! count (+ 1 count))
      (string-append "count-" (number->string count)))))

(define count counter)

(display (string-append "Expect 'count-1': " (count) "\n"))
(display (string-append "Expect 'count-2': " (count) "\n"))
(display (string-append "Expect 'count-3': " (count) "\n"))
(display (string-append "Expect 'count-4': " (count) "\n"))
