;; Mickey Scheme library structure

;; Libraries loaded by REPL
(define-repl-imports
  (scheme base)
  (scheme cxr)
  (scheme write)
  (scheme char)
  (scheme load)
  (scheme process-context)
  (scheme repl))

;; List of available libraries and their locations
(define-library-index
  ((c stdio)                 "c/stdio.scm")
  ((cross-platform sdl)      "cross-platform/sdl.scm")
  ((experimental endianness) "experimental/endianness.scm")
  ((maclisp)                 "maclisp/maclisp.scm")
  ((mickey environment)      "mickey/environment.scm")
  ((mickey gensym)           "mickey/gensym.scm")
  ((mickey internals)        "mickey/internals.scm")
  ((mickey library)          "mickey/library.scm")
  ((mickey misc)             "mickey/misc.scm")
  ((portable atom)           "portable/atom.scm")
  ((portable flatten)        "portable/flatten.scm")
  ((scheme base)             "scheme/base.scm")
  ((scheme char)             "scheme/char.scm")
  ((scheme cxr)              "scheme/cxr.scm")
  ((scheme lazy)             "scheme/lazy.scm")
  ((scheme load)             "scheme/load.scm")
  ((scheme math)             "scheme/math.scm")
  ((scheme process-context)  "scheme/process-context.scm")
  ((scheme repl)             "scheme/repl.scm")
  ((scheme write)            "scheme/write.scm")
  ((srfi 1)                  "srfi/srfi-1-reference.scm")
  ((test unit-test)          "test/unit-test.scm")
  ((unix dlopen)             "")
  ((unix uname)              "unix/uname.scm"))
