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

;; Call curl_easy_init first
(define init
  (call-foreign-function
    (prepare-call-interface 'default-abi 'pointer '())
    (dlsym lib "curl_easy_init")
    8))
(println "curl_easy_init() ==> " (return-value->pointer init))

(define cif
  (prepare-call-interface 'default-abi 'uchar))

(define val
  (call-foreign-function cif funptr 8))

(println fname "() ==> '" (return-value->string val) "'")
