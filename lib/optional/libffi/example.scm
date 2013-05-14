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

  ;; The code below is really terrible; functions redefining
  ;; themselves. It's just that I don't like the idiom
  ;;
  ;; (define foo #f)
  ;; (let ...
  ;;   (set! foo (lambda () ...)))

  (define curl-easy-init
    (let*
      ((fptr (dlsym curl "curl_easy_init"))
       (cif (prepare-call-interface 'default-abi 'pointer)))
      (if (not fptr) (error "Could not find curl_easy_init"))
      (lambda ()
        (set! curl-easy-init (lambda ()
                               (return-value->pointer
                                 (call-foreign-function
                                   cif fptr (size-of 'pointer)))))
        (curl-easy-init))))

  (define curl-version
    (let*
      ((fptr (dlsym curl "curl_version"))
       (cif (prepare-call-interface 'default-abi 'pointer)))
      (if (not fptr) (error "Could not find curl_version"))
      (lambda ()
        (set! curl-version (lambda ()
                             (return-value->string
                               (call-foreign-function
                                 cif fptr (size-of 'pointer)))))
        (curl-version))))

  ;; MAIN CODE
  (curl-easy-init)
  (println "libcurl version: " (curl-version)))
