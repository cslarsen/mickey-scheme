(define call/cc call-with-current-continuation)
(define continuation #f)
(define counter 0)

(begin
  (display
    (* 2 (call/cc (lambda (ret)
                    (set! continuation ret)
                    (ret 100)))))
  (newline)) ; ==> 2*100 == 200
  
(continuation 2) ; ==> 4
(continuation 7) ; ==> 14
(continuation 10) ; ==> 20
(continuation 30) ; ==> 60
