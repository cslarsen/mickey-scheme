(import (prefix (crypto gnupg) gpg-)
        (scheme base)
        (scheme write)
        (scheme char)
        (scheme cxr)
        (posix time)
        (portable print))

(define (capitalize s)
  (string-append
    (char-upcase (string-ref s 0))
    (substring s 1 (- (string-length s) 1))))

(define (to-string x)
  (cond
    ((symbol? x) (capitalize (symbol->string x)))
    ((boolean? x) (if x "Yes" "No"))
    ((number? x) (number->string x))
    (else x)))

(println "Using GPGME version " (gpg-version) "\n")

(define (indent n)
  (make-string n #\space))

(define (display-alist x)
  (cond
    ((null? x) '())
    ((symbol? x) (print (capitalize (symbol->string x)) ": "))
    ((list? x)
     (begin
       (if (and (symbol? (car x))
                (list? (cadr x)))
         (begin
           (println "==" (to-string (car x)) "==")
           (display-alist (cdr x)))
         (begin
           (display-alist (car x))
           (if (or (eqv? (car x) 'timestamp)
                   (eqv? (car x) 'expires))
             (print (ctime (time (cadr x)))) ;; Convert to time
             (display-alist (cdr x)))))))
    (else
      (println (to-string x)))))

(for-each
  (lambda (kv)
    (display-alist kv)
    (println))
  (gpg-public-keys))
