#|

This is a cleaner, more comfortable interface to libffi. It consists of
wrapper code around (ffi libffi).

Copyright (C) 2013 Christian Stigen Larsen
Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0

|#
(define-library (ffi)
  (import (scheme base)
          (ffi libffi)
          (unix dlopen))

  (export

  (begin
    (define (open-library file options)
      (apply dlopen (cons file options)))

    (define (lookup-function library function-name)
      (dlsym library function-name))

    (define make-signature
      (case-lambda
        (() (make-signature 'void '(void) 'default-abi))

        ((rettype/argtypes
          (case rettype/argtypes
            (list?   (make-signature 'void rettype/argtypes))
            (symbol? (make-signature rettype/argtypes '(void)))
            (else    (error "Invalid argument")))))

        ((rettype argtypes)
          (make-signature 'default-abi rettype argtypes))

        ((return-type argument-types call-abi)
           (make-interface return-type
                           argument-types
                           abi))))

    (define (call signature function size-of-return-value)
      (call-function signature
                     function
                     size-of-return-value))))

#|

NOTE:

What I really want is something as simple as

  (define http-get
    (bind-ff foo-library
             '(void http_get (char* char* char*))))

which would then be used like

  (http-get "http://www.nytimes.com" "user" "pass")
  (http-get "http://www.iht.com" "user" "pass")

The big point is the function signature

  '(<return-value-type> <dlsym-name> (<type-arg1> <type-arg2> ...))

A similar method could easily be used to describe structs, e.g.

  struct tm {
    int tm_sec;
    int tm_min;
    ...
    long int __tm_gmtoff__;
    __const char *__tm_zone__;
  };

could be described like

  (make-type '(int int slong pointer) 0 0)

This returns an ffi_type object with the given structure and size=0 and
alignment=0.

|#
