#|

Mickey R7RS Scheme

Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#
(define-library (scheme time)
  (import (only (scheme base) define)
          (mickey library))

  (export
    current-jiffy
    current-second
    jiffies-per-second)

  (begin
    (open-internal-library-determine-extension "libscheme-time" 'lazy 'global)
    (define current-second (bind-procedure "proc_current_second"))
    (define current-jiffy (bind-procedure "proc_current_jiffy"))
    (define jiffies-per-second (bind-procedure "proc_jiffies_per_second"))))
