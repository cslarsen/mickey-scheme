(import (scheme base)
        (scheme write))

(define-record-type piece
  (piece type color)
  piece?
  (type piece-type set-piece-type!)
  (color piece-color set-piece-color!))

(define a (piece 'pawn 'white))
(display "Piece type: ")
(display (piece-type a))
(newline)
(display "Piece color: ")
(set-piece-color! a 'black)
(set-piece-type! a 'queen)
(display (piece-color a))
(newline)
