#|
   SRFI-1 list-processing library for R7RS Scheme

   Copyright (C) 1998, 1999 by Olin Shivers (Original SRFI-1)
   Copyright (C) 2012 by Christian Stigen Larsen (R7RS library format)

   Distributed under the GNU LGPL 2.1; see LICENSE

|#
(define-library (srfi 1)
  (import (scheme base)
          (scheme cxr))

  (include "srfi-1-reference.scm")

  (export
    alist-cons
    alist-copy
    alist-delete
    alist-delete!
    any
    append!
    append-map
    append-map!
    append-reverse
    append-reverse!
    break
    break!
    car+cdr
    circular-list
    circular-list?
    concatenate
    concatenate!
    cons*
    count
    delete
    delete!
    delete-duplicates
    delete-duplicates!
    dotted-list?
    drop
    drop-right
    drop-right!
    drop-while
    eigth
    every
    fifth
    filter
    filter!
    filter-map
    find
    find-tail
    first
    fold
    fold-right
    fourth
    iota
    last
    last-pair
    length+
    list-copy
    list-index
    list-tabulate
    list=
    lset-adjoin
    lset-diff+intersection
    lset-diff+intersection!
    lset-difference
    lset-difference!
    lset-intersection
    lset-intersection!
    lset-union
    lset-union!
    lset-xor
    lset-xor!
    lset<=
    lset=
    make-list
    map!
    map-in-order
    ninth
    not-pair?
    null-list?
    pair-fold
    pair-fold-right
    pair-for-each
    partition
    partition!
    proper-list?
    reduce
    reduce-right
    remove
    remove!
    reverse!
    second
    seventh
    sixth
    span
    span!
    split-at
    split-at!
    take
    take!
    take-right
    take-while
    take-while!
    tenth
    third
    unfold
    unfold-right
    unzip1
    unzip2
    unzip3
    unzip4
    unzip5
    xcons
    zip)
  (begin
    ;; Taken from comments in srfi-1-reference.scm
    (define (check-arg pred val caller)
      (let lp ((val val))
        (if (pred val) val
          (lp (error "Bad argument" val pred caller)))))))
