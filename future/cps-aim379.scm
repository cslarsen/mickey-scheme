; The code below was taken from
;
;   Guy L. Steele Jr., AI MEMO 379,
;     "LAMBDA: The Ultimate Declarative" (November 1976), Appendix A, p. 30:
;     http://dspace.mit.edu/bitstream/handle/1721.1/6091/AIM-379.pdf
;
; Translated to Mickey R7RS Scheme by Christian Stigen Larsen, December 2012
;
; I've tried adding a (maclisp) library, but some forms cannot be parsed, like
; the "(,CONT ,SEXPR) form, without modifying the reader, and there is no such
; support in Mickey yet.
;
; Note that the (maclisp) library has some bugs in it, and more to the point,
; the PUTPROP and GET functions are currently incorrect, and does not work for
; general purpose objects.  Therefore, the code as-is does not run properly.

(import (maclisp))

;; Must define gentempnum because of a macro environment bug
(define gentempnum '())

#!fold-case

;; Appendix A.  Conversion to Continuaion-Passing Style
;; ----------------------------------------------------
;;
;; Here we present a set of functions, written in SCHEME, which convert a
;; SCHEME expression form functional style to pure continuation-passing
;; style.  {Note PLASMA CPS}

(ASET' GENTEMPNUM 0)

(DEFINE GENTEMP
  (LAMBDA (X)
    (IMPLODE (CONS X (EXPLODEN (ASET' GENTEMPNUM (+ GENTEMPNUM 1)))))))

(DEFINE CPS
  (LAMBDA (SEXPR)
    (SPRINTER (CPC SEXPR NIL '#CONT#))))

;; CPS (Continuation-Passing Style) is the main function;  its argument is
;; the expression to be converted.  It calls CPC (C-P Conversion) to do the
;; real work, and then calls SPRINTER to pretty-print the result, for
;; convenience.  The symbol #CONT# is used to represent the implied
;; continuation which is to receive the value of the expression.

(DEFINE CPC
  (LAMBDA (SEXPR ENV CONT)
    (COND ((ATOM SEXPR) (CPC-ATOM SEXPR ENV CONT))
          ((EQ (CAR SEXPR) 'QUOTE)
           (IF CONT `(,CONT ,SEXPR) SEXPR))
          ((EQ (CAR SEXPR) 'LAMBDA)
           (CPC-LAMBDA SEXPR ENV CONT))
          ((EQ (CAR SEXPR) 'IF)
           (CPC-IF SEXPR ENV CONT))
          ((EQ (CAR SEXPR) 'CATCH)
           (CPC-CATCH SEXPR ENV CONT))
          ((EQ (CAR SEXPR) 'LABELS)
           (CPC-LABELS SEXPR ENV CONT))
          ((AND (ATOM (CAR SEXPR))
                (GET (CAR SEXPR) 'AMACRO))
           (CPC (FUNCALL (GET (CAR EXPR) 'AMACRO) SEXPR) ENV CONT))
          (T (CPC-FORM SEXPR ENV CONT)))))

;; CPC merely dispatches to one of a number of subsidiary routines based on
;; the form of the expression SEXPR.  ENV represents the environment in
;; which SEXPR will be evaluated;  it is a list of the variable names.  When
;; CPS initially calls CPC, ENV is NIL.  CONT is the continuation which will
;; receive the value of SEXPR.  The double-quote (") is like a single-quote,
;; except that within the quoted expression any subexpressions preceded by
;; comma (,) are evaluated and substituted in (also, any subexpressions
;; preceded by atsign (@) are substituted in a list segments).  One special
;; case handled directly by CPC is a quoted expression;  CPC also expands
;; any SCHEME macros encountered.

(DEFINE CPC-ATOM
  (LAMBDA (SEXPR ENV CONT)
    ((LAMBDA (AT) (IF CONT `(,CONT ,AT) AT))
     (COND ((NUMBERP SEXPR) SEXPR)
           ((MEMQ SEXPR ENV) SEXPR)
           ((GET SEXPR 'CPS-NAME))
           (T (IMPLODE (CONS '% (EXPLODEN SEXPR))))))))

;; For convenience, CPC-ATOM will change the name of a global atom.  Numbers
;; and atoms in the environment are not changed;  otherwise, a specified
;; name on the property list of the given atom is used (properties defined
;; below convert "+" into "++", etc.);  otherwise, the name is prefixed with
;; "%".  Once the name has been converted, it is converted to a form which
;; invokes the continuation on the atom.  (If a null continuation is
;; supplied, the atom itself is returned.)

(DEFINE CPC-LAMBDA
  (LAMBDA (SEXPR ENV CONT)
    ((LAMBDA (CN)
        ((LAMBDA (LX) (IF CONT `(,CONT ,LX) LX))
         `(LAMBDA (@(CADR SEXPR) ,CN)
                  ,(CPC (CADDR SEXPR)
                        (APPEND (CADR SEXPR) (CONS CN ENV))
                        CN))))
     (GENTEMP 'C))))

;; A LAMBDA expression must have an additional parameter, the continuation
;; supplied to its body, added to its parameter list.  CN holds the name of
;; this generated parameter.  A new LAMBDA expression is created, with CN
;; added, and with its body converted in an environment containing the new
;; variables.  Then the same test for a null CONT is made as in CPC-ATOM.

(DEFINE CPC-IF
  (LAMBDA (SEXPR ENV CONT)
    ((LAMBDA (KN)
      `((LAMBDA (,KN)
                ,(CPC (CADR SEXPR)
                      ENV
                      ((LAMBDA (PN)
                          `(LAMBDA (,PN)
                              (IF ,PN
                                  ,(CPC (CADDR SEXPR)
                                        ENV
                                        KN)
                                  ,(CPC (CADDDR SEXPR)
                                        ENV
                                        KN))))
                       (GENTEMP 'P))))
        ,CONT))
     (GENTEMP 'K))))

;; First, the continuation for an IF must be given a name KN (rather, the
;; name held in KN;  but for convenience, we will continue to use this
;; ambiguity, for the form of the name is indeed Kn for some number n), for
;; it will be referred to in two places and we wish to avoid duplicating the
;; code.  Then, the predicate is converted to continuation-passing style,
;; using a continuation which will receive the result and call it PN.  This
;; continuation will then use an IF to decide which converted consequent to
;; invoke.  Each consequent is converted using continuation KN.

(DEFINE CPC-CATCH
  (LAMBDA (SEXPR ENV CONT)
          ((LAMBDA (ENV)
                   `((LAMBDA (,EN)
                             ((LAMBDA (,(CADR SEXPR))
                                      ,(CPC (CADDR SEXPR)
                                            (CONS (CADR SEXPR) ENV)
                                            EN))
                              (LAMBDA (V C) (,EN V))))
                     ,CONT))
           (GENTEMP 'E))))

;; This routine handles CATCH as defined in [Sussman 75], and in converting
;; it to continuation-passing style eliminates all occurrences of CATCH.
;; The idea is to give the continuation a name EN, and to bind the CATCH
;; variable to a continuation (LAMBDA (V C) ...)  which ignores its
;; continuation and instead exits the catch by calling EN with its argument
;; V.  The body of the CATCH is converyed using continuation EN.

(DEFINE CPC-LABELS
        (LAMBDA (SEXPR ENV CONT)
                (DO ((X (CADR SEXPR) (CDR X))
                     (Y ENV (CONS (CAAR X) Y)))
                    ((NULL X)
                     (DO ((W (CADR SEXPR) (CDR W))
                          (Z NIL (CONS (LIST (CAAR W)
                                             (CPC (CADAR W) Y NIL))
                                       Z)))
                         ((NULL W)
                          `(LABELS ,(REVERSE Z)
                                   ,(CPC (CADDR SEXPR) Y CONT))))))))

;; Here we have used DO loops as defined in MacLISP (DO is implemented as a
;; macro in SCHEME).  There are two passes, one performed by each DO.  The
;; first pass merely collects in Y the names of all the labelled LAMBDA
;; expressions.  The second pass converts all the LAMBDA expressions using a
;; null continuation and an environment augmented by all the collected names
;; in Y, collecting them in Z.  At the end, a new LABELS is constructed
;; using the results in Z and a converted LABELS body.

(DEFINE CPC-FORM
  (LAMBDA (SEXPR ENV CONT)
          (LABELS ((LOOP1
                    (LAMBDA (X Y Z)
                            (IF (NULL X)
                                (DO ((F (REVERSE (CONS CONT Y))
                                        (IF (NULL (CAR Z)) F
                                            (CPC (CAR Z)
                                                 ENV
                                                 `(LAMBDA (,(CAR Y)) ,F))))
                                     (Y Y (CDR Y))
                                     (Z Z (CDR Z)))
                                    ((NULL Z) F))
                                (COND ((OR (NULL (CAR X))
                                           (ATOM (CAR X)))
                                       (LOOP1 (CDR X)
                                              (CONS (CPC (CAR X) ENV NIL) Y)
                                              (CONS NIL Z)))
                                      ((EQ (CAAR X) 'QUOTE)
                                       (LOOP1 (CDR X)
                                              (CONS (CAR X) Y)
                                              (CONS NIL Z)))
                                      ((EQ (CAAR X) 'LAMBDA)
                                       (LOOP1 (CDR X)
                                              (CONS (CPC (CAR X) ENV NIL) Y)
                                              (CONS NIL Z)))
                                      (T (LOOP1 (CDR X)
                                                (CONS (GENTEMP 'T) Y)
                                                (CONS (CAR X) Z))))))))
                  (LOOP1 SEXPR NIL NIL))))

;; This, the most complicated routine, converts forms (function calls).
;; This also operates in two passes.  The first pass, using LOOP1, uses X to
;; step down the expression, collecting data in Y and Z.  At each step, if
;; the next element of X can be evaluated trivially, then it is converted
;; with a null continuation and added to Y, and NIL is added to Z.
;; Otherwise, a temporary name TN for the result of the subexpresion is
;; created and put in Y, and the subexpression itself is put in Z.  ON the
;; second pass (the DO loop), the final continuation-passing form is
;; constructed in F from the inside out.  At each step, if the element of Z
;; is non-null, a new continuation must be created.  (There is actually a
;; bug in CPC-FORM, which has to do with variables affected by side-effects.
;; This is easily fixed by changing LOOP1 so that it generates temporaries
;; for variables even though variables evaluate trivially.  This would only
;; obscure the examples presented below, however, and so this was omitted.)

(LABELS ((BAR
          (LAMBDA (DUMMY X Y)
                  (IF (NULL X) '|CPS ready to go!|
                      (BAR (PUTPROP (CAR X) (CAR Y) 'CPS-NAME)
                           (CDR X)
                           (CDR Y))))))
        (BAR NIL
             '(+ - * // ^ T NIL)
             '(++ -- ** //// ^^ 'T 'NIL)))

;; This loop sets up some properties so that "+" will translate into "++"
;; instead of "%+", etc.
;;
;; Now let us examine some examples of the action of CPS.  First, let us try
;; our old friend FACT, the iterative factorial program.

(DEFINE FACT
        (LAMBDA (N)
                (LABELS ((FACT1 (LAMBDA (M A)
                                        (IF (= M 0) A
                                            (FACT1 (- M 1) (* M A))))))
                        (FACT1 N 1))))

;; Applying CPS to the LAMBDA expression for FACT yields:

(CPS (QUOTE
  (DEFINE FACT
         (LAMBDA (N)
                 (LABELS ((FACT1 (LAMBDA (M A)
                                         (IF (= M 0) A
                                             (FACT1 (- M 1) (* M A))))))
                         (FACT1 N 1))))))

;; {Note PLASMA CPS}
;;
;; Hewitt has performed similar experimentso n PLASMA programs [Hewitt 76],
;; by converting PLASMA programs to a form which uses only ==> and <==
;; transmission arrows.  A subsequent uniform replacement of these arrows by
;; => and <= preserves the semantics of the programs.
