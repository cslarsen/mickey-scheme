;; Example of loading libcurl using libffi throuh Mickey Scheme.
;; By Christian Stigen Larsen
;;
;; NOTE: Does not work, yet.

(import (ffi libffi)
        (scheme base)
        (scheme write)
        (portable print)
        (unix dlopen))

(define (find-library path library-name)
  (string-append
    path "/" library-name
    (cond-expand
      (linux ".so")
      (darwin ".dylib")
      (else ".so"))))

(println "Example of using (ffi libffi) with libcurl")

(let*
  ((path (find-library "/usr/lib" "libcurl"))
   (_ (println "Loading " path))
   (curl (dlopen path 'now 'global)))

  (if (not curl) (error "Could not load libcurl"))

  ;; SET UP SOME PROCEDURES

  (define curl-easy-init #f)

  (let*
    ((fptr (dlsym curl "curl_easy_init"))
     (cif (prepare-call-interface 'default-abi 'pointer)))
    (if (not fptr) (error "Could not find curl_easy_init"))
    (set! curl-easy-init
      (lambda ()
        (return-value->pointer
          (call-foreign-function cif fptr (size-of 'void*))))))

  (define curl-version #f)

  (let*
    ((fptr (dlsym curl "curl_version"))
     (cif (prepare-call-interface 'default-abi 'pointer)))
    (if (not fptr) (error "Could not find curl_version"))
    (set! curl-version
      (lambda ()
        (return-value->string
          (call-foreign-function cif fptr (size-of 'char*))))))

  ;; MAIN CODE
  (curl-easy-init)
  (println "libcurl version: " (curl-version)))
