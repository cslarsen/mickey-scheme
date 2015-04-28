;; Testing (cond) in Mickey Scheme
(import (scheme base))
(import (scheme write))

(define (who-is name)
  (cond ((equal? name "Dave")    "Sandy's boyfriend. Organizer of the rescue effort")
        ((equal? name "Syd")     "an aspiring musician trying to start his own new-wave band")
        ((equal? name "Michael") "an award winning photographer for the college newspaper")
        ((equal? name "Wendy")   "hoping to be a famour novelist and is waitin for a big break")
        ((equal? name "Bernard") "president of the physics club and winner of the college's Geek Award")
        ((equal? name "Razor")   "lead singer for the punk band 'Razor and the Scummettes'")
        ((equal? name "Jeff")    "usually hanging out on the beach, responds to the name Surfer Dude")
        (else "someone I don't know")))

(define (tell-me-about name)
  (display (string-append
    name " is " (who-is name) ".\n")))

(tell-me-about "Syd")
(tell-me-about "Dave")
(tell-me-about "Razor")
(tell-me-about "Purple Tentacle")
