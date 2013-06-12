(import (scheme base)
        (scheme write)
        (scheme inquiry))

(display `(Running on ,(implementation-name)
                      ,(implementation-version)))
(newline)

(display `(CPU architecture: ,(cpu-architecture)))
(newline)

(display `(C memory model: ,(c-memory-model)))
(newline)

(display `(System instance: ,(system-instance)))
(newline)

(display `(OS type: ,(os-type)))
(newline)

(display `(OS version: ,(os-version)))
(newline)
