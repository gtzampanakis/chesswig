(import (util))
(import (chesswig))
(import (tests))

(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")
(define fen-mate-in-2 "r1b2k1r/ppp1bppp/8/1B1Q4/5q2/2P5/PPP2PPP/R3R1K1 w - - 1 0")
(define fen-mate-in-2-simplified
  "5k1n/4bpp1/8/1B1Q4/8/8/5PPP/4R1KN w - - 1 1")
(define fen-promotion "7k/P7/8/8/8/8/7p/K7 w - - 0 1")
(define fen-stupid-take "k7/8/8/8/7p/6p1/8/K4N2 w - - 0 1")
  
(define fen-simple "6nk/8/8/8/8/8/8/KN6 w - - 0 1")

(define fen-en-passant "5r2/7p/3R4/p3pk2/1p2N2p/1P2BP2/6PK/4r3 w - - 1 0")

(define (port-first-line port)
  (let loop ((c (read-char port)) (line '()))
    (if (or (eof-object? c) (char=? #\newline c))
      (list (list->string (reverse line)) (if (eof-object? c) 'done port))
      (loop (read-char port) (cons c line)))))

(define (port-all-lines port)
  (let loop ((line+port (port-first-line port)) (lines '()))
    (let ((line (car line+port)) (port (cadr line+port)))
      (if (equal? port 'done)
        (reverse lines)
        (loop (port-first-line port) (cons line lines))))))

(define (is-line-fen? line)
  (let ((line-list (string->list line)))
    (=
      7
      (let loop ((n 0) (line-list line-list))
        (if (null? line-list)
          n
          (loop
            (+ n
              (if (char=? #\/ (car line-list)) 1 0))
            (cdr line-list)))))))

(define (is-line-move-list? line)
  (let ((line-list (string->list line)))
    (if (null? line-list)
      #f
      (if (char=? (car line-list) #\1)
        #t
        #f))))

(define (main)

  ;(display-eval-obj
  ;  position
  ;  (evaluate-position-at-ply position 3/2))

  ;(define position-2
  ;  (position-after-move position
  ;    (list P (cls-to-coords (list 6 1)) (cls-to-coords (list 6 3)) '())))
  ;
  ;(display (position-en-passant position))(newline)
  ;(display (coords-to-cls (position-en-passant position-2)))(newline)
  ;;(display-eval-obj
  ;;  position
  ;;  (evaluate-position-at-ply position-2 1/2))
  ;(display 'legal-moves-running-now)(newline)
  ;(display (legal-moves position-2 #f))(newline)
  ;(display 'legal-moves-done-running)(newline)

  ;(define lines
  ;  (call-with-input-file "mates_in_2.txt"
  ;    (lambda (port)
  ;      (port-all-lines port))))
  ;(for-each
  ;  (lambda (line)
  ;    (if (is-line-fen? line)
  ;      (let* (
  ;          (pos (decode-fen line))
  ;          (eval-obj (evaluate-position-at-ply pos 3/2)))
  ;        (display line)(newline)
  ;        (display-move-seq pos (cadar eval-obj))(newline))
  ;      (unless (is-line-move-list? line)
  ;        (display line)(newline))))
  ;  lines)

  ;(define position (decode-fen fen-promotion))
  (define position
    (decode-fen fen-mate-in-2))

  (unless (run-tests)
    (raise "tests-failed"))

  (display "Started")(newline)

  (let ((eval-obj (evaluate-position-at-ply position 3/2 #f)))
      (display-eval-obj position eval-obj 32))
  (newline)

  (when track-positions-examined?
    (display "Number of positions examined: ")
    (display (car n-positions-examined))
    (newline))

  (display "Done")(newline)

)

(main)
