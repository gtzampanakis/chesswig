(import (match match) (util))
(import (chesswig))

(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")
(define fen-mate-in-2 "r1b2k1r/ppp1bppp/8/1B1Q4/5q2/2P5/PPP2PPP/R3R1K1 w - - 1 0")
(define fen-simple "6nk/8/8/8/8/8/8/KN6 w - - 0 1")

(define (main)
  (define position
    (decode-fen fen-mate-in-2))

  (display-eval-obj
    position
    (evaluate-position-at-ply position 1/2 'admit-all #t))

  ;(when track-positions-examined?
  ;  (d "Positions examined:" (hashtable-size positions-examined)))

  )

(main)
