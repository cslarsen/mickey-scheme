#|

Mickey R7RS Scheme

Copyright (C) 2012-2013 Christian Stigen Larsen <csl@sublevel3.org>
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
    emergency-exit
    exit
    delete-file
    file-exists?
    get-environment-variable
    get-environment-variables)

  (begin
    (open-internal-library "libscheme-process-context.so" 'global 'lazy)

    (define command-line (bind-procedure "proc_command_line"))
    (define exit (bind-procedure "proc_exit"))
    (define emergency-exit (bind-procedure "proc_emergency_exit"))
    (define file-exists? (bind-procedure "proc_file_existsp"))
    (define delete-file (bind-procedure "proc_delete_file"))
    (define get-environment-variable (bind-procedure "proc_get_environment_variable"))
    (define get-environment-variables (bind-procedure "proc_get_environment_variables"))))
