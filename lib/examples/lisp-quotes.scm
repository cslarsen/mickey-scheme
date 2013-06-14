;; A collection of Lisp quotes.
;;
;; Part of Mickey Scheme
;;
;; Copyright (C) 2013 Christian Stigen Larsen
;; Distributed under the LGPL 2.1; see the LICENSE file

(define-library (examples lisp-quotes)
  (import (scheme base)
          (scheme write)
          (scheme time)
          (portable prng lcg))

  (export
    random-quote
    all-quotes
    set-prng)

  (begin
    (define prng (lcg-glibc (current-second)))

    (define (set-prng prng-proc)
      (if (procedure? prng-proc)
        (set! prng prng-proc)
        (error "Not a procedure")))

    (define quotes '(
      ((quote
         "Lisp is a programmable programming language.")
       (author "John Foderaro")
       (source "CACM, September 1991"))

      ((quote
         "Pascal is for building pyramids --- imposing, breathtaking, static\n"
         "structures built by armies pushing heavy blocks into place.\n"
         "\n"
         "Lisp is for building organisms [...]")
       (author "Alan J. Perlis")
       (source "Foreword to SICP"))

      ((quote
         "Programs must be written for people to read, and only incidentally for\n"
         "machines to execute.")
       (author "Abelson & Sussman")
       (source "Preface to SICP, 1st ed."))

      ((quote
         "[Lisp is the] greatest single programming language ever designed.")
       (author "Alan Kay")
       (source "\"Daddy, Are We There Yet?\", A discussion with Alan Kay"))

      ((quote
         "Lisp has jokingly been called \"the most intelligent way to misuse a\n"
         "computer\"\n"
         "\n"
         "I think that description is a great compliment because it transmits the full\n"
         "flavor of liberation: it has assisted a number of our most gifted fellow\n"
         "humans in thinking previously impossible thoughts.")
       (author "Edsger W. Dijkstra")
       (source "\"The Humble Programmer\", 1972 Turing Award Lecture"))

      ((quote
         "I finally understood that the half page of code on the bottom of page 13 of\n"
         "the Lisp 1.5 manual was Lisp in itself.\n"
         "\n"
         "These were \"Maxwell’s Equations of Software!\"")
       (author "Alan Kay")
       (source "\"A Conversation with Alan Kay, ACM Queue 2 (9)"))

      ((quote
         "LISP is worth learning for a different reason --- the profound enlightenment\n"
         "experience you will have when you finally get it. That experience will make\n"
         "you a better programmer for the rest of your days, even if you never\n"
         "actually use LISP itself a lot.")
       (author "Eric S. Raymond")
       (source "\"How to Become a Hacker\""))

      ((quote
         "Schemer: \"Buddha is small, clean, and serious.\"\n"
         "Lispnik: \"Buddha is big, has hairy armpits, and laughs.\"")
       (author "Nikodemus Siivola"))

      ((quote
         "The continuation that obeys only obvious stack semantics, O grasshopper,\n"
         "is not the true continuation.")
       (author "Guy L. Steele, Jr."))

      ((quote
         "I have heard more than one LISP advocate state such subjective comments as,\n"
         "\"LISP is the most powerful and elegant programming language in the world\"\n"
         "and expect such comments to be taken as objective truth. I have never heard\n"
         "a Java, C++, C, Perl, or Python advocate make the same claim about their own\n"
         "language of choice.")
       (author "A guy on Slashdot"))

      ((quote
         "To understand a program, you must become both the machine and the program.")
       (author "Alan Perlis")
       (source "\"Epigrams in programming\", ACM SIGPLAN, September 1982"))

      ((quote
         "You're posting to a Scheme group. Around here, arguing that Java is better\n"
         "than C++ is like arguing that grasshoppers taste better than tree bark.")
       (author "Thant Tessman")
       (source "comp.lang.scheme"))

      ((quote
         "If you give someone Fortran, he has Fortran.\n"
         "If you give someone Lisp, he has any language he pleases.")
       (author "Guy L. Steele, Jr."))

      ((quote
        "Parentheses?  What parentheses? I haven't noticed any parentheses since my\n"
        "first month of Lisp programming.  I like to ask people who complain about\n"
        "parentheses in Lisp if they are bothered by all the spaces between words in\n"
        "a newspaper.")
       (author "Kenny Tilton"))

      ((quote
         "I am reminded of Gregor Kiczales at ILC 2003 [the International Lisp\n"
         "Conference] displaying some AspectJ to a silent crowd, pausing, then\n"
         "plaintively adding, \"When I show that to Java programmers they stand\n"
         "up and cheer.\"")
       (author "Kenny Tilton"))

    )

    (define (all-quotes) quotes)

    (define (lookup key alist)
      (let
        ((value (assq key alist)))
        (if value (cadr value) "")))

    (define (has-key key alist)
      (if (assq key alist) #t #f))

    (define (random-quote)
      (let*
        ((no (modulo (prng) (length quotes)))
         (data (list-ref quotes no)))
        (string-append
          (apply string-append (cdr (assq 'quote data)))
          "\n"
          "      -- " (if (has-key 'author data)
                        (lookup 'author data)
                        "Unknown")
          (if (positive? (string-length (lookup 'source data)))
            (string-append ", " (lookup 'source data)) ""))))))
