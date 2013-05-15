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

  (define curlopt-url 10002) ; taken from g++ -E some-curl-proj.cpp
  (define curl-easy-setopt #f)
  (let*
    ((fptr (dlsym curl "curl_easy_setopt"))
     (cif (make-interface
            'default-abi
            'sint
            '(pointer sint pointer))))

    (if (not fptr) (error "Could not find curl_easy_setopt"))

    (set! curl-easy-setopt
      (lambda (handle option data)
        (value->integer
          (call-function cif fptr (size-of 'sint)
                         (list handle option data))))))

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

  (define (check-result code)
    (if (> code 0)
      (println "Error: " (curl-easy-strerror code))))

  ;; MAIN CODE
  (define handle (curl-easy-init))
  (println (curl-version))
  (check-result (curl-easy-setopt handle
                                  curlopt-url
                                  "http://www.google.com")))
