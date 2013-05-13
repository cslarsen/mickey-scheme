(import (ffi libffi)
        (scheme base)
        (scheme write)
        (portable print)
        (unix dlopen))

(define path "/usr/lib/libcurl.dylib")

(println "Loading " path)
(define lib (dlopen path 'now 'global))

(define fname "curl_version")
(define funptr (dlsym lib fname))

(if (not funptr)
  (error (string-append
           "Could not find function " fname)))

(define cif
  (prepare-call-interface 'default-abi 'uchar))

(println "Calling foreign function")
(define val
  (call-foreign-function cif funptr 32))

(println fname "() ==> " (return-value->string val))
