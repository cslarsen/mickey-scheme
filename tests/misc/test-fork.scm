(import (scheme base)
        (portable print)
        (posix exit)
        (posix fork)
        (posix wait))

(println "fork(2) example in Mickey Scheme")

(define (child-process)
  (let ((exitcode 123))
    (println "child>   Hello from the child process!")
    (println "child>   Exiting with status " exitcode)
    (exit exitcode)))

(let ((pid (fork)))
  (if (child-process? pid) (child-process)
      (begin
        (println "parent>  The child process is " pid)

        (let* ((pid_status (wait))
               (retpid (car pid_status))
               (status (cadr pid_status)))

          (println "parent>  Was child signaled? " (signaled? status))
          (println "parent>  Was child exited? " (exited? status))
          (println "parent>  Child " retpid " exited with status "
                   (exitstatus status))))))

