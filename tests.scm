#!chezscheme

(library (tests)
(export
  run-tests
)
(import (chezscheme) (chesswig))

(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")
(define fen-mate-in-2 "r1b2k1r/ppp1bppp/8/1B1Q4/5q2/2P5/PPP2PPP/R3R1K1 w - - 1 0")
(define fen-mate-in-2-simplified
  "5k1n/4bpp1/8/1B1Q4/8/8/5PPP/4R1KN w - - 1 1")
(define fen-promotion "7k/P7/8/8/8/8/7p/K7 w - - 0 1")
(define fen-stupid-take "k7/8/8/8/7p/6p1/8/K4N2 w - - 0 1")
(define fen-simple "6nk/8/8/8/8/8/8/KN6 w - - 0 1")
(define fen-en-passant "5r2/7p/3R4/p3pk2/1p2N2p/1P2BP2/6PK/4r3 w - - 1 0")

(define (assert-equal a b)
  (let ((r (equal? a b)))
    (unless r
      (raise-continuable (list a "!=" b)))))

(define (test-simple-mate-in-2)
  (define position
    (decode-fen fen-mate-in-2-simplified))
  (let ((eval-obj (evaluate-position-at-ply position 3/2 #f)))
      (assert-equal (length eval-obj) 50)))

(define (run-tests)
  (define r #t)
  (with-exception-handler
    (lambda (e)
      (set! r #f)
      (display (append (list "test failed:" "test-simple-mate-in-2") e)))
    test-simple-mate-in-2)
  r)

)
