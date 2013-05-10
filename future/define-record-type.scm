(import (scheme base)
        (scheme cxr)
        (scheme write)
        (portable sequence)
        (portable print)
        (portable gensym))

;; Helper-function to gensym record type identifiers so that they're
;; essentially generative (meaning, each use produces a NEW type).
;;
;; TODO: Move into (portable gensym) or something like that.
;;

(define-macro define-record-type record-def
  (let*
    ((<def>        record-def)
     (<name>       (car <def>))
     (<ctor>       (cadr <def>))
     (<predicate?>  (caddr <def>))
     (<field-defs> (cdddr <def>))
     (<fields>     (length <field-defs>))
     (<id>         (string->symbol
                   (string-append
                     "record-type-" (symbol->string <name>) "-"
                     (symbol->string (gensym))))))

    (println
      "name: " <name> "\n"
      "ctor: " <ctor> "\n"
      "pred: " <predicate?> "\n"
      "fields: " <field-defs> "\n"
      "field no: " <fields> "\n"
      "id: " <id> "\n"
      "1st field name: '" (car (list-ref <field-defs> 0)) "'\n"
      "2nd field name: '" (car (list-ref <field-defs> 1)) "'\n")

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
             (accessor (cadr field) index))
           <field-defs>)))

    (define mutators
      (let ((index -1))
        (map
          (lambda (field)
            (if (>= (length field) 3)
                (begin
                  (set! index (+ 1 index))
                  (mutator (caddr field) index))))
          <field-defs>)))

    (define final-code
      `(begin
        ,constructor
        ,predicate
        ,@accessors
        ,@mutators))

    final-code))

(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(define v (kons 22 33))
(println
  "kar: " (kar v) "\n"
  "kdr: " (kdr v))

(println "Setting kar to 999")

(set-kar! v 999)

(println
  "kar: " (kar v) "\n"
  "kdr: " (kdr v))
