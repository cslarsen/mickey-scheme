(import (scheme base)
        (scheme write)
        (test unit-test))

(testq (let ((|foo bar| 123)) |foo bar|) 123)
(testq (let ((|foo bar| 123)) (* 2 |foo bar|)) 246)
(testq (let ((baz '|foo bar baz|)) baz) (quote |foo bar baz|))

(testq
  (let
    ((|squared number| (lambda (x) (* x x))))
    (|squared number| 12)) 144)

(tap-results)
