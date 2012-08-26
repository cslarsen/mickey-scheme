(import (unix dlopen)
        (scheme write)
        (scheme base))

(define sdl (dlopen "src/libmickey-sdl.so" 'lazy))
(define initialize (dlsym sdl "initialize"))
(define set-video-mode (dlsym sdl "set_video_mode"))

(initialize)
(set-video-mode 640 480 'hwsurface 'fullscreen)
