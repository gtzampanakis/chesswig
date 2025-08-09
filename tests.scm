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

(define (uniq-sort ls)
  (let ((sorted (sort < ls)))
    (reverse
      (fold-left
        (lambda (acc el)
          (if (null? acc)
            (cons el acc)
            (if (equal? (car acc) el)
              acc
              (cons el acc))))
        '()
        sorted))))

(define-condition-type &test-failure &condition make-test-failure test-failure?
  (desc-ls test-failure-desc-ls))

(define-syntax run-test
  (syntax-rules ()
    ((_ test-name result-var)
      (with-exception-handler
        (lambda (e)
          (if (test-failure? e)
            (begin
              (set! result-var #f)
              (display
                (append
                  (list "test failed:" (quote test-name))
                  (test-failure-desc-ls e)))
              (newline))
            (raise e)))
        test-name))))

(define (run-tests)
  (define r #t)
  (run-test test-simple-mate-in-2 r)
  (run-test test-king-can-be-exposed r)
  (run-test test-is-position-check r)
  (run-test test-is-position-checkmate r)
  (run-test test-is-position-stalemate r)
  (run-test test-decode-encode-fen r)
  (run-test test-updates-to-legal-moves-of-rook-caused-by-move r)
  (run-test test->desc-args->position-1 r)
  r)

(define (assert-equal a b)
  (let ((r (equal? a b)))
    (unless r
      (raise-continuable (make-test-failure (list a "!=" b))))))

(define (test-simple-mate-in-2)
  (define position
    (decode-fen fen-mate-in-2-simplified))
  (let ((moves (legal-moves position)))
      (assert-equal (length moves) 50)))

(define (test-king-can-be-exposed)
  (let* (
    (position (decode-fen "k7/8/8/8/8/2b5/1Q6/K7 w - - 0 1"))
    (moves (legal-moves position)))
      (assert-equal (length moves) 3))
  (let* (
    (position (decode-fen "k7/8/8/8/8/2q5/1Q6/K7 w - - 0 1"))
    (moves (legal-moves position)))
      (assert-equal (length moves) 3))
  (let* (
    (position (decode-fen "8/K1k5/P7/8/8/8/8/8 w - - 0 1"))
    (moves (legal-moves position)))
      (assert-equal (length moves) 1))
  (let* (
    (position (decode-fen "k7/8/8/8/3b4/2n5/1Q6/K7 w - - 0 1"))
    (moves (legal-moves position)))
      (assert-equal (length moves) 17))
  (let* (
    (position (decode-fen "k7/8/2p1p3/2P1P3/3K4/8/2p2p2/8 w - - 0 1"))
    (moves (legal-moves position)))
      (assert-equal (length moves) 5))
  (let* (
    (position (decode-fen "k2q4/8/2p1p3/n1P1P3/3K4/7r/2pn1p2/8 w - - 0 1"))
    (moves (legal-moves position)))
      (assert-equal (length moves) 0))
)

(define (test-is-position-check)
  (let* (
    (position (decode-fen "8/8/4p3/3K4/8/8/8/k7 w - - 0 1")))
      (assert-equal (is-position-check? position) #t)))

(define (test-is-position-checkmate)
  (let* (
    (position (decode-fen "4n3/3b1p2/1p2p3/3K2n1/8/2nk4/8/4r3 w - - 0 1")))
      (assert-equal (is-position-checkmate? position) #t)))

(define (test-is-position-stalemate)
  (let* (
    (position (decode-fen "4n3/3bpp2/1p6/3K2n1/8/3k4/1n6/4r3 w - - 0 1")))
      (assert-equal (is-position-stalemate? position) #t)))

(define (test-decode-encode-fen)
  (let* (
    (fen "4n3/3bpp2/1p6/3K2n1/8/3k4/1n6/4r3 w - - 0 1"))
      (assert-equal (encode-fen (decode-fen fen)) fen)))

(define (test->desc-args->position-1)
  (assert-equal
    (encode-fen
      (desc-args->position
        `(
          (,K "a1")
          (,k "a8")
          (,R "e5")
          (,R "h4"))
        'w))
    "k7/8/8/4R3/7R/8/8/K7 w - - 0 1"))

(define (test-updates-to-legal-moves-of-rook-caused-by-move)
  (let* (
      (position
        (desc-args->position
          `(
            (,K "a1")
            (,k "a8")
            (,R "e5")
            (,R "h4")
            (,r "h3")
            (,Q "h7"))
          'w)))
    (display-position position)
    (assert-equal
      (updates-to-legal-moves-caused-by-move 
        position
        (piece-algs->move position "h4" "h5"))
      (list
        (list (alg->coords "h3") dir-d)
        (list (alg->coords "h7") dir-u)
        (list (alg->coords "e5") dir-l)
        (list (alg->coords "h3") dir-d)
        (list (alg->coords "h7") dir-u)))))

)
