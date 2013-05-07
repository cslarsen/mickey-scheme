#|

Simple (and incomplete) chess engine written in portable R7RS scheme.

Written by Christian Stigen Larsen
Put in the public domain by the author.

Version 2013-05-07

|#

(import (scheme base)
        (scheme write)
        (scheme char))

;; Some constants

(define files '(a b c d e f g h))
(define ranks '(1 2 3 4 5 6 7 8))

(define piece-chars
  '((#f     #\.)
    (pawn   #\p)
    (knight #\n)
    (bishop #\b)
    (rook   #\r)
    (queen  #\q)
    (king   #\k)))

(define (make-board)
  "Returns an empty board."
  (make-vector 64 #f))

(define (make-default-board)
  "Returns a properly set up board."
  (list->vector
    '((white . rook) (white . knight) (white . bishop) (white . queen)
      (white . king) (white . bishop) (white . knight) (white . rook)
      (white . pawn) (white . pawn)   (white . pawn)   (white . pawn)
      (white . pawn) (white . pawn)   (white . pawn)   (white . pawn)
      #f #f #f #f #f #f #f #f
      #f #f #f #f #f #f #f #f
      #f #f #f #f #f #f #f #f
      #f #f #f #f #f #f #f #f
      (black . pawn) (black . pawn)   (black . pawn)   (black . pawn)
      (black . pawn) (black . pawn)   (black . pawn)   (black . pawn)
      (black . rook) (black . knight) (black . bishop) (black . queen)
      (black . king) (black . bishop) (black . knight) (black . rook))))

(define (make-piece color type)
  "Create a piece with a color and type."
  (cons color type))

(define (type piece)
  "Returns the piece's type (e.g., pawn, etc.)."
  (if piece (cdr piece) #f))

(define (color piece)
  "Returns the piece's color."
  (if piece (car piece) #f))

(define (file->integer file)
  "Convert a file to an integer."
  (cadr (assv file ; TODO: (zip files ranks)
              '((a 1) (b 2) (c 3) (d 4)
                (e 5) (f 6) (g 7) (h 8)))))

(define (position file rank)
  "Create a position made of a file and rank."
  (cons file rank))

(define (file position)
  "Get the file part of a position."
  (car position))

(define (rank position)
  "Get the rank part of a position."
  (cdr position))

(define (position->index pos)
  "Convert position to index into the board vector."
  (- (+ (file->integer (file pos))
        (* 8 (- (rank pos) 1)))
     1))

(define (index-ok? index)
  "Return #t if index is within board bounds."
  (and (>= index 0) (< index 64)))

(define (board-ref board position)
  "Return piece at given position."
  (let
    ((index (position->index position)))
    (if (index-ok? index)
        (vector-ref board (position->index position))
        #f)))

(define (board-set! board position piece)
  "Set piece at given position"
  (let
    ((index (position->index position)))
    (if (index-ok? index)
      (vector-set! board index piece))))

(define (white? piece)
  (eqv? 'white (color piece)))

(define (black? piece)
  (eqv? 'black (color piece)))

(define (piece->char piece)
  ((if (white? piece) char-downcase char-upcase)
   (cadr (assv (type piece) piece-chars))))

(define (print-board board)
  (define (print-piece position)
    (display " ")
    (display (piece->char (board-ref board position))))

  (define (print-rank rank)
    (display rank)
    (for-each
      (lambda (file)
        (print-piece (position file rank)))
      files)
    (newline))

  (begin
    (for-each print-rank (reverse ranks))
    (display "  a b c d e f g h")
    (newline)))

(define board (make-default-board))
(print-board board)
