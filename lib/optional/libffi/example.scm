;; Example of loading libcurl using libffi throuh Mickey Scheme.
;; By Christian Stigen Larsen
;;
;; You must have a libffi version of at least 3.0.11 (Apr-11-12), as we
;; require the support for variadic functions (ffi_prep_cif_var).

(import (ffi libffi)
        (scheme base)
        (scheme write)
        (portable print)
        (unix dlopen))

(define (find-library path library-name)
  (string-append
    path "/" library-name
    (cond-expand
      (linux  ".so")
      (darwin ".dylib")
      (else   ".so"))))

(println "Example of using libffi with libcurl")
(println "libffi version " (version))
(println)

(let*
  ((path (find-library "/usr/lib" "libcurl"))
   (_    (println "Loading " path))
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
       (cif (make-interface 'default-abi 'pointer)))

      (if (not fptr)
        (error "Could not find curl_easy_init"))

      (lambda ()
        (set! curl-easy-init
          (lambda ()
            (value->pointer
              (call-function cif fptr (size-of 'pointer)))))
        (curl-easy-init))))

  (define curl-version
    (let*
      ((fptr (dlsym curl "curl_version"))
       (cif (make-interface 'default-abi 'pointer)))

      (if (not fptr)
        (error "Could not find curl_version"))

      (lambda ()
        (set! curl-version
          (lambda ()
            (value->string
              (call-function cif fptr (size-of 'pointer)))))
        (curl-version))))

  ;; Find enum value by doing:
  ;; gcc -E /usr/include/curl/curl.h | grep CURLOPT_URL
  ;;
  (define curlopt-url 10002)

  (define curl-easy-setopt #f)
  (let*
    ((fptr (dlsym curl "curl_easy_setopt"))
     (cif (make-variadic-interface
            'default-abi
            'sint
            2
            '(pointer sint pointer))))

    (if (not fptr) (error "Could not find curl_easy_setopt"))

    (set! curl-easy-setopt
      (lambda (handle option value)
        (value->integer
          (call-function cif fptr (size-of 'sint)
                         (list handle option value))))))

  (define curl-easy-strerror #f)
  (let*
    ((fptr (dlsym curl "curl_easy_strerror"))
     (cif (make-interface 'default-abi 'pointer '(sint))))
    (if (not fptr) (error "Could not find curl_easy_strerror"))
    (set! curl-easy-strerror
      (lambda (error-code)
        (value->string
          (call-function cif fptr (size-of 'pointer)
                         (list error-code))))))

  (define curl-easy-perform #f)
  (let*
    ((fptr (dlsym curl "curl_easy_perform"))
     (cif (make-interface 'default-abi 'sint '(pointer))))
    (if (not fptr) (error "Could not find curl_easy_perform"))
    (set! curl-easy-perform
      (lambda (error-code)
        (value->integer
          (call-function cif fptr (size-of 'pointer)
                         (list error-code))))))

  (define (check-result code)
    (if (> code 0)
      (println "Error: " (curl-easy-strerror code))))

  ;; MAIN CODE
  (define handle (curl-easy-init))
  (println (curl-version))
  (check-result (curl-easy-setopt handle
                                  curlopt-url
                                  "http://www.google.com"))
  (curl-easy-perform handle))
