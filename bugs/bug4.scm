(import 
        (prefix
          (rename
            (scheme base)
            (when TEST-WHEN)
            (unless TEST-UNLESS))
          base:)
        (rename (scheme write)
                (display print-this)))

(print-this "Howdy!\n")

(print-this (base:TEST-WHEN #t "Yes, it works"))
(base:newline)

(print-this "This should be empty ==> ")
(print-this (base:TEST-WHEN #f "No, it does NOT work"))
(base:newline)

; The following does not work, because "unless"
; is implemented in terms of (not), which is now
; called base:not.  This is weird, because internally,
; it should be able to call its own not.
;

#|
 | Altså, her er det bug på gang.
 |
 | Det burde gå FINT an å lage en (unless) som bruker (not)
 | i base-lib, selv om den blir renamet eksternt.
 |
 | Første environment har normale navn, og closures som peker
 | på sitt eget miljø.
 |
 | Man lager et nytt miljø og kopierer over disse closures
 | men med nye navn og eksporterer dette.
 |
 | Det er ingenting galt med dette, fordi closures skal
 | evalueres i sitt opprinnelige miljø.  Så her er det en
 | bug på gang!
 |#

(define we-dont-want-to-print #f)

(print-this "And ALSO this ==> ")
(print-this (base:TEST-UNLESS we-dont-want-to-print "YEAH!"))
(base:newline)
