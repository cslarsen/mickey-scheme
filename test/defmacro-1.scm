(import (scheme base)
        (scheme write)
        (scheme cxr))

(define-macro define-my-record-type rec
  `(begin
     (display
       (let
         ((name (car rec))
          (ctor (cadr rec))
          (pred (caddr rec)))
         (list "define-record-type called with\n"
               "  name: " name "\n"
               "  ctor: " ctor "\n"
               "  pred: " pred "\n")))
     (newline)))

(define-my-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))
