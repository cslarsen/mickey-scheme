#|

Mickey R7RS Scheme

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme process-context)

  (import (only (scheme base) define)
          (mickey library))

  (export
    command-line
    exit
    get-environment-variable
    get-environment-variables)

  (begin
    (open-internal-library "libscheme-process-context.so")

    (define command-line
      (bind-procedure "proc_command_line"))

    (define exit
      (bind-procedure "exit"))

    (define get-environment-variable
      (bind-procedure "proc_get_environment_variable"))

    (define get-environment-variables
      (bind-procedure "proc_get_environment_variables"))))
