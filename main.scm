(import (util))
(import (chesswig))

(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")
(define fen-mate-in-2 "r1b2k1r/ppp1bppp/8/1B1Q4/5q2/2P5/PPP2PPP/R3R1K1 w - - 1 0")
(define fen-mate-in-2-simplified
  "5k1n/4bpp1/8/1B1Q4/8/8/5PPP/4R1KN w - - 1 1")
(define fen-promotion "7k/P7/8/8/8/8/7p/K7 w - - 0 1")
  
(define fen-simple "6nk/8/8/8/8/8/8/KN6 w - - 0 1")

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
  ;(define position
  ;  (decode-fen fen-mate-in-2-simplified))

  ;(display-eval-obj
  ;  position
  ;  (evaluate-position-at-ply position 1/2))

  (define lines
    (call-with-input-file "mates_in_2.txt"
      (lambda (port)
        (port-all-lines port))))

  (for-each
    (lambda (line)
      (if (is-line-fen? line)
        (let* (
            (pos (decode-fen line))
            (eval-obj (evaluate-position-at-ply pos 3/2)))
          (display line)(newline)
          (display-move-seq pos (cadar eval-obj))(newline))
        (unless (is-line-move-list? line)
          (display line)(newline))))
    (take lines 200))

  ;(define position (decode-fen fen-promotion))

  ;(define eval-obj (evaluate-position-at-ply position 1/2))
  ;(display-eval-obj position eval-obj)

  (display "Done")(newline)

)

(main)
