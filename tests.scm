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
  (let ((moves (legal-moves position #f)))
      (assert-equal (length moves) 50)))

(define (test-king-can-be-exposed)
  (let* (
    (position (decode-fen "k7/8/8/8/8/2b5/1Q6/K7 w - - 0 1"))
    (moves (legal-moves position #f)))
      (assert-equal (length moves) 3))
  (let* (
    (position (decode-fen "k7/8/8/8/8/2q5/1Q6/K7 w - - 0 1"))
    (moves (legal-moves position #f)))
      (assert-equal (length moves) 3))
  (let* (
    (position (decode-fen "8/K1k5/P7/8/8/8/8/8 w - - 0 1"))
    (moves (legal-moves position #f)))
      (assert-equal (length moves) 1))
  (let* (
    (position (decode-fen "k7/8/8/8/3b4/2n5/1Q6/K7 w - - 0 1"))
    (moves (legal-moves position #f)))
      (assert-equal (length moves) 17))
  (let* (
    (position (decode-fen "k7/8/2p1p3/2P1P3/3K4/8/2p2p2/8 w - - 0 1"))
    (moves (legal-moves position #f)))
      (assert-equal (length moves) 5))
  (let* (
    (position (decode-fen "k2q4/8/2p1p3/n1P1P3/3K4/7r/2pn1p2/8 w - - 0 1"))
    (moves (legal-moves position #f)))
      (assert-equal (length moves) 0))
)

(define (run-tests)
  (define r #t)
  (with-exception-handler
    (lambda (e)
      (set! r #f)
      (display (append (list "test failed:" "test-simple-mate-in-2") e))
      (newline))
    test-simple-mate-in-2)
  (with-exception-handler
    (lambda (e)
      (set! r #f)
      (display (append (list "test failed:" "test-king-can-be-exposed") e))
      (newline))
    test-king-can-be-exposed)
  r)

)
