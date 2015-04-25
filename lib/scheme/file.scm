#|

Mickey R7RS Scheme

Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme file)

  (import (only (scheme base) define)
          (mickey library))

  (export
    file-exists?
    delete-file)

  (begin
    (open-internal-library-determine-extension "libscheme-file" 'global 'lazy)

    (define file-exists? (bind-procedure "proc_file_existsp"))
    (define delete-file (bind-procedure "proc_delete_file"))))
