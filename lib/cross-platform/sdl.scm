#|

Copyright (C) 2012 Christian Stigen Larsen
http://csl.sublevel3.org

Distributed under the LGPL 2.1; see LICENSE

|#

(define-library (cross-platform sdl)
  (export
    initialize
    set-video-mode)

  (import (only (scheme base) define)
          (mickey library))

  (begin
    (open-internal-library-determine-extension
      "libcross-platform-sdl" 'lazy 'global)
    (define initialize (bind-procedure "initialize"))
    (define set-video-mode (bind-procedure "set_video_mode"))))
