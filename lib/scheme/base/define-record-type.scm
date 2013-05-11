#|

Example define-record-type implementation

Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
http://csl.sublevel3.org                              _
                                                       \
Distributed under the LGPL 2.1; see LICENSE            /\
Please post bugfixes and suggestions to the author.   /  \_

WEAKNESSES / TODO

  - Field accessors/mutators must appear in same order as
    in constructor. Fix this by building a name->index map.

  - Structure is clearly visible and mutatable by programs.
    Fix this by storing it in a "pointer-type" object or
    similar.

  - Readd missing gensym. Can't make them generative without it.

|#

;; Helper function taken from (portable sequence)
;;

(define-macro define-record-type record-def
  (let*
    ((<def>        record-def)
     (<name>       (car <def>))
     (<ctor>       (car (cdr <def>)))
     (<predicate?> (car (cdr (cdr <def>))))
     (<field-defs> (cdr (cdr (cdr <def>))))
     (<fields>     (length <field-defs>))
     (<id>         (string->symbol
                     (string-append
                       "record-type-" (symbol->string <name>) "-"
                       "<missing-gensym>")))

     ;; Sanity checks

     (if (null? <def>) (error "Missing record definition"))

     (if (null? <name>) (error "Missing record name"))
     (if (not (symbol? <name>)) (error "Invalid record name"))

     (if (null? <ctor>) (error "Missing record constructor"))
     (if (not (and (list? <ctor>) ;; must be a list ...
                   ;(apply and (map symbol? <ctor>)) ;; ... of only symbols
                   (>= 1 (length <ctor>)) ; must have at least one member
                   (symbol? (car <ctor>)))) ; must be a symbol
       (error "Invalid constructor signature"))

     (if (null? <predicate?>) (error "Missing predicate procedure name"))

     (seq
       (lambda (start stop)
         (let
           ((out '()))

           (let loop
             ((cur start))
             (if (<= cur stop)
                 (begin
                   (set! out (append out (list cur)))
                   (loop (+ 1 cur)))))
           out))))

    (define constructor
      `(define ,<ctor>
         (let
           ; Record structure with id and field values
           ((<struct> (list (quote ,<id>) (make-vector ,<fields> #f))))

           ; Initialize fields
           ,@(map
               (lambda (index)
                 `(vector-set! (cadr <struct>)
                               ,index
                               ,(list-ref <ctor> (+ 1 index))))
               (seq 0 (- <fields> 1)))
           <struct>)))

    (define (accessor name index)
      `(define (,name <struct>)
         (if (,<predicate?> <struct>)
             (vector-ref (cadr <struct>) ,index)
             (error (string-append "Not a record of type "
                                   (symbol->string ,<name>))))))

    (define (mutator name index)
      `(define (,name <struct> <value>)
         (if (,<predicate?> <struct>)
             (vector-set! (cadr <struct>) ,index <value>)
             (error (string-append "Not a record of type "
                                   (symbol->string ,<name>))))))

    (define predicate
      `(define (,<predicate?> <struct>)
         (and (list? <struct>)
              (eqv? (car <struct>) (quote ,<id>))
              (vector? (cadr <struct>))
              (= (vector-length (cadr <struct>)) ,<fields>))))

    (define accessors
      (let ((index -1))
        (map
           (lambda (field)
             (set! index (+ 1 index))
             (accessor (car (cdr field)) index))
           <field-defs>)))

    (define mutators
      (let ((index -1))
        (map
          (lambda (field)
            (if (>= (length field) 3)
                (begin
                  (set! index (+ 1 index))
                  (mutator (car (cdr (cdr field))) index))))
          <field-defs>)))

    (define final-code
      `(begin
        ,constructor
        ,predicate
        ,@accessors
        ,@mutators))

    final-code))
