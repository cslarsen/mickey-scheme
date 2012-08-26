#|

Mickey R7RS Scheme

Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

|#

(define-library (scheme char)
  (import (only (scheme base) defien)
          (mickey library))

  (export
    char-alphabetic? char-downcase
    char-lower-case? char-numeric?
    char-upcase char-upper-case?
    char-whitespace?
    char-foldcase)

  (begin
    #|
       Basically all the functionality in this library resides in the shared
       object file below.
    |#
    (open-internal-library "libscheme-char.so")

    (define char-alphabetic? (bind-procedure "proc_char_alphabeticp"))
    (define char-downcase    (bind-procedure "proc_char_downcase"))
    (define char-lower-case? (bind-procedure "proc_char_lowercasep"))
    (define char-numeric?    (bind-procedure "proc_char_numericp"))
    (define char-upcase      (bind-procedure "proc_char_upcase"))
    (define char-upper-case? (bind-procedure "proc_char_uppercasep"))
    (define char-whitespace? (bind-procedure "proc_char_whitespacep"))

    #|
       From R7RS draft 6:

       "[...] applies the unicode simple case-folding algorithm to
        its argument and returns the result."

       For details, see the Unicode Standard Annex #29 at
       http://unicode.org/reports/tr29/

       Note that THIS implementation is CHEATING, and does NOT employ the
       correct algorithm.  Implementations are allowed to only support the
       ASCII portion of UTF-8, though.
    |#
    (define (char-foldcase char)
      ; simply call `char-downcase`
      ; note that this might not be entirely correct
      (char-downcase char))))
