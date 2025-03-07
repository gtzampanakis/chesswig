#!chezscheme

(library (chesswig)
(export
  encode-fen
  decode-fen
  display-move-seq
  display-eval-obj
  evaluate-position-at-ply
  legal-moves
)
(import (chezscheme) (util) (unpack))

(define caching? #t)

(define P-base 1)
(define R-base 2)
(define N-base 3)
(define B-base 4)
(define Q-base 5)
(define K-base 6)

(define P P-base)
(define R R-base)
(define N N-base)
(define B B-base)
(define Q Q-base)
(define K K-base)
(define E 9)
(define p (+ E P-base))
(define r (+ E R-base))
(define n (+ E N-base))
(define b (+ E B-base))
(define q (+ E Q-base))
(define k (+ E K-base))

(define dir-u 0)
(define dir-r 1)
(define dir-d 2)
(define dir-l 3)
(define dir-ur 4)
(define dir-dr 5)
(define dir-dl 6)
(define dir-ul 7)
(define dir-nur 8)
(define dir-nru 9)
(define dir-nrd 10)
(define dir-ndr 11)
(define dir-ndl 12)
(define dir-nld 13)
(define dir-nlu 14)
(define dir-nul 15)

(define all-directions 
  (list
    dir-u dir-r dir-d dir-l
    dir-ur dir-dr dir-dl dir-ul
    dir-nur dir-nru dir-nrd dir-ndr
    dir-ndl dir-nld dir-nlu dir-nul))

(define knight-directions
  (list
    dir-nur dir-nru dir-nrd dir-ndr
    dir-ndl dir-nld dir-nlu dir-nul))

(define rook-directions
  (list dir-u dir-r dir-d dir-l))

(define bishop-directions
  (list dir-ur dir-dr dir-dl dir-ul))

(define queen-directions
  (list dir-u dir-r dir-d dir-l dir-ur dir-dr dir-dl dir-ul))

(define king-directions
  (list dir-u dir-r dir-d dir-l dir-ur dir-dr dir-dl dir-ul))

(define positions-examined
  (make-hashtable string-hash string=?))

(define track-positions-examined? #f)

(define (char->piece char)
  (case char
    ((#\R) R)
    ((#\N) N)
    ((#\B) B)
    ((#\Q) Q)
    ((#\K) K)
    ((#\P) P)
    ((#\r) r)
    ((#\n) n)
    ((#\b) b)
    ((#\q) q)
    ((#\k) k)
    ((#\p) p)))

(define (piece->char piece)
  (cond
    ((= piece R) #\R)
    ((= piece N) #\N)
    ((= piece B) #\B)
    ((= piece Q) #\Q)
    ((= piece K) #\K)
    ((= piece P) #\P)
    ((= piece r) #\r)
    ((= piece n) #\n)
    ((= piece b) #\b)
    ((= piece q) #\q)
    ((= piece k) #\k)
    ((= piece p) #\p)))

(define-record-type position
  (fields
    placement
    active-color
    castling
    en-passant
    halfmoves
    fullmoves
    parent-position
    parent-move
    (mutable moves)
    (mutable static-val)
    (mutable eval-at-ply)
    (mutable check)
    (mutable can-king-be-captured))
  (protocol
    (lambda (p)
      (lambda (placement active-color castling
                en-passant halfmoves
                fullmoves parent-position parent-move)
        (let ((obj
                (p placement active-color castling
                    en-passant halfmoves
                    fullmoves parent-position parent-move
                    '() '() '() '() '())))
          obj)
        ))))

; cls means: "coords list" and it's a list of file and rank.
; coords means: a number encoding a cls
(define (coords-to-cls coords)
  (let-values (((q rem) (div-and-mod coords 8)))
    (let ((f rem) (r q))
      (list f r))))

(define (cls-to-coords cls)
  (let ((f (car cls)) (r (cadr cls)))
    (+ (* 8 r) f)))

(define (memoized-proc getter setter proc)
; Memoization that stores its data inside the first argument. This allows to
; have a separate cache for each position which means that we do not need to
; cache the first argument which might be expensive because of its complexity.
; It also causes positions to be garbage collected together with their caches.
  (lambda args
    (if (not caching?)
      (apply proc args)
      (let ((position (car args)) (rest (cdr args)))
        (let ((c (getter position)))
          (let ((cached-result-pair (assoc rest c)))
            (if cached-result-pair
              (cdr cached-result-pair)
              (let ((result (apply proc args)))
                (setter position (cons (cons rest result) c))
                result))
            ))))))

(define all-coords (iota 64))

(define (placement-ref position coords)
  (bytevector-u8-ref (position-placement position) coords))

(define (map-over-placement include-white? include-black? proc position)
  (apply append
    (map
      (lambda (color-wanted)
        (define cs all-coords)
        (let loop ((result '()) (trimmed '()) (cs cs))
          (if (null? cs)
            result
            (let* (
                (coords (car cs))
                (piece-found (piece-at-coords position coords))
                (color-found (piece-color piece-found)))
              (let ((correct-color? (symbol=? color-found color-wanted)))
                (loop
                  (if correct-color?
                    (cons (proc piece-found coords) result)
                    result)
                  (if correct-color?
                    (cons coords trimmed)
                    trimmed)
                  (cdr cs)))))))
      (append
        (if include-white? (list 'w) '())
        (if include-black? (list 'b) '())))))

(define (for-each-over-placement proc position)
  (for-each
    (lambda (coords)
      (proc (placement-ref position coords) coords))
    all-coords))

(define (char->symbol char)
  (string->symbol (string char)))

(define (alg-to-square alg)
  (let* (
      (chars (string->list alg))
      (file-char (car chars))
      (rank-char (cadr chars)))
    (list
      (case file-char
        ((#\a) 0)
        ((#\b) 1)
        ((#\c) 2)
        ((#\d) 3)
        ((#\e) 4)
        ((#\f) 5)
        ((#\g) 6)
        ((#\h) 7))
      (case rank-char
        ((#\1) 0)
        ((#\2) 1)
        ((#\3) 2)
        ((#\4) 3)
        ((#\5) 4)
        ((#\6) 5)
        ((#\7) 6)
        ((#\8) 7)))))

(define (file-to-alg file)
  (case file
    ((0) "a")
    ((1) "b")
    ((2) "c")
    ((3) "d")
    ((4) "e")
    ((5) "f")
    ((6) "g")
    ((7) "h")))

(define (rank-to-alg rank)
  (case rank
    ((0) "1")
    ((1) "2")
    ((2) "3")
    ((3) "4")
    ((4) "5")
    ((5) "6")
    ((6) "7")
    ((7) "8")))

(define (square-to-alg sq)
  (string-append
    (file-to-alg (car (coords-to-cls sq)))
    (rank-to-alg (cadr (coords-to-cls sq)))))

(define (is-move-capture? position move)
  (piece-at-coords? position (caddr move)))

(define (move-to-alg position move)
  (define piece-moving (car move))
  (define sq-from (cadr move))
  (define sq-to (caddr move))
  (define sq-to-str (square-to-alg sq-to))
  (define promotion-str
    (let ((promotion-piece (list-ref move 3)))
      (cond
        ((null? promotion-piece) "")
        ((= promotion-piece R) "=R")
        ((= promotion-piece r) "=R")
        ((= promotion-piece N) "=N")
        ((= promotion-piece n) "=N")
        ((= promotion-piece B) "=B")
        ((= promotion-piece b) "=B")
        ((= promotion-piece Q) "=Q")
        ((= promotion-piece q) "=Q"))))
  (define capture? (is-move-capture? position move))
  (define capture-str (if capture? "x" ""))
  (define piece-moving-str
    (cond
      ((or (= piece-moving P) (= piece-moving p))
        (if capture?
          (file-to-alg (car (coords-to-cls sq-from)))
          ""))
      ((or (= piece-moving N) (= piece-moving n)) "N")
      ((or (= piece-moving B) (= piece-moving b)) "B")
      ((or (= piece-moving Q) (= piece-moving q)) "Q")
      ((or (= piece-moving R) (= piece-moving r)) "R")
      ((or (= piece-moving K) (= piece-moving k)) "K")
      (else (debug))))
  (define rank-str
    (call/1cc
      (lambda (cont)
        (for-each-over-placement
          (lambda (piece coords)
            (when (and (not (= piece P)) (not (= piece p)))
              (when (= piece piece-moving)
                (when (= (car (coords-to-cls coords)) (car (coords-to-cls sq-from)))
                  ; When same type of piece on same file.
                  (when (not (equal? coords sq-from))
                    (for-each
                      (lambda (sq)
                        (when (equal? sq sq-to)
                          (cont
                            (rank-to-alg
                              (cadr (coords-to-cls sq-from))))
                          (exit)))
                      (legal-squares-from-coords
                           position piece coords)))))))
          position)
        "")))
  (define file-str
    (call/1cc
      (lambda (cont)
        (for-each-over-placement
          (lambda (piece coords)
            (when (and (not (= piece P)) (not (= piece p)))
              (when (= piece piece-moving)
                (when (not (= (car (coords-to-cls coords)) (car (coords-to-cls sq-from))))
                  ; When same type of piece on different file.
                  (when (not (equal? coords sq-from))
                    (for-each
                      (lambda (sq)
                        (when (equal? sq sq-to)
                          (cont
                            (file-to-alg
                              (car (coords-to-cls sq-from))))
                          (exit)))
                      (legal-squares-from-coords
                           position piece coords)))))))
          position)
        "")))
  (define next-position (position-after-move position move))
  (define check-or-checkmate-str
    (cond
      ((is-position-checkmate? next-position) "#")
      ((is-position-check? next-position) "+")
      (else "")))
  (define result
    (string-append
      piece-moving-str
      file-str
      rank-str
      capture-str
      sq-to-str
      promotion-str
      check-or-checkmate-str))
  result)

(define (white-piece? piece) (< piece E))

(define (black-piece? piece) (> piece E))

(define (piece-color piece)
  (cond
    ((white-piece? piece) 'w)
    ((black-piece? piece) 'b)
    (else 'empty)))

(define (toggled-color color)
  (if (symbol=? color 'w) 'b 'w))

(define (decode-rank rank-string)
  (fold-left
    (lambda (current-result char)
      (case char
        ((#\r #\n #\b #\q #\k #\p #\R #\N #\B #\Q #\K #\P)
          (cons
            (char->piece char)
            current-result))
        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8)
          (append
            (make-list (string->number (string char)) E)
            current-result))))
    '()
    (string->list rank-string)))

(define (piece-at-coords position coords)
  (placement-ref position coords))

(define (decode-placement-data placement-data-string)
  (u8-list->bytevector
    (reverse
      (let ((rank-strings (string-split placement-data-string #\/)))
        (apply append (map decode-rank rank-strings))))))

(define (decode-active-color active-color-string)
  (cond
    ((equal? active-color-string "w") 'w)
    ((equal? active-color-string "b") 'b)))

(define (decode-castling castling-string)
  (if (equal? castling-string "-")
    '()
    (map char->symbol (string->list castling-string))))

(define (decode-en-passant en-passant-string)
  (cond
    ((equal? en-passant-string "-") '())
    (else (alg-to-square en-passant-string))))

(define (decode-halfmoves halfmoves-string)
  (string->number halfmoves-string))

(define (decode-fullmoves fullmoves-string)
  (string->number fullmoves-string))

(define (position-copy-w-toggled-active-color position)
; think cases where toggling the active color results in an invalid position,
; for example when a check exists.
  (let ((color (position-active-color position)))
    (make-position
      (position-placement position)
      (if (symbol=? color 'w) 'b 'w)
      (position-castling position)
      (position-en-passant position)
      (position-halfmoves position)
      (position-fullmoves position)
      #f
      #f
      )))

(define (decode-fen fen-string)
  (let ((field-strings (string-split fen-string #\ )))
    (let ((placement (decode-placement-data (list-ref field-strings 0))))
      (make-position
        placement
        (decode-active-color (list-ref field-strings 1))
        (decode-castling (list-ref field-strings 2))
        (decode-en-passant (list-ref field-strings 3))
        (decode-halfmoves (list-ref field-strings 4))
        (decode-fullmoves (list-ref field-strings 5))
        #f #f
        ))))

(define (inc-char char)
  (car
    (string->list
      (number->string
        (1+
          (string->number
            (string char)))))))

(define (symbol->chars sym)
  (string->list (symbol->string sym)))

(define (encode-fen position)
  (define placement (position-placement position))
  (define active-color (position-active-color position))
  (define castling (position-castling position))
  (define en-passant (position-en-passant position))
  (define halfmoves (position-halfmoves position))
  (define fullmoves (position-fullmoves position))
  (define placement-list (bytevector->u8-list placement))
  (define placement-chars
    (let loop ((r 7) (result '()) (placement-list placement-list))
      (if (= r -1)
        result
        (loop
          (1- r)
          (append
            (reverse
              (fold-left
                (lambda (previous piece)
                  (cond
                    ((= piece E)
                      (cond
                        ((null? previous)
                          (cons #\1 previous))
                        (else
                          (case (car previous)
                             ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8)
                            (cons
                              (inc-char (car previous))
                              (cdr previous)))
                             (else (cons #\1 previous))))))
                    (else (cons (piece->char piece) previous))))
                '()
                (take placement-list 8)))
            (if (= r 7) (list) (list #\/))
            result)
          (drop placement-list 8)))))
  (apply
    string
    (append
      placement-chars
      (list #\ )
      (symbol->chars active-color)
      (list #\ )
      (if (null? castling)
        (list #\-)
        (apply
          append
          (map symbol->chars castling)))
      (list #\ )
      (string->list
        (if (null? en-passant) "-" (square-to-alg en-passant)))
      (list #\ )
      (string->list (number->string halfmoves))
      (list #\ )
      (string->list (number->string fullmoves)))))

(define (calc-next-coords-in-direction coords direction)
  (let-values (((prov-f prov-r)
      (let* (
          (f (car (coords-to-cls coords)))
          (r (cadr (coords-to-cls coords))))
        (cond
          ((= direction dir-u) (values f (1+ r)))
          ((= direction dir-r) (values (1+ f) r))
          ((= direction dir-d) (values f (1- r)))
          ((= direction dir-l) (values (1- f) r))
          ((= direction dir-ur) (values (1+ f) (1+ r)))
          ((= direction dir-dr) (values (1+ f) (1- r)))
          ((= direction dir-dl) (values (1- f) (1- r)))
          ((= direction dir-ul) (values (1- f) (1+ r)))
          ; Knight directions: The first character gives the
          ; two-square movement and the second character the
          ; one-square move. For example, 'ndr means "knight move,
          ; first two squares down then one square right".
          ((= direction dir-nur) (values (+ f 1) (+ r 2)))
          ((= direction dir-nru) (values (+ f 2) (+ r 1)))
          ((= direction dir-nrd) (values (+ f 2) (- r 1)))
          ((= direction dir-ndr) (values (+ f 1) (- r 2)))
          ((= direction dir-ndl) (values (- f 1) (- r 2)))
          ((= direction dir-nld) (values (- f 2) (- r 1)))
          ((= direction dir-nlu) (values (- f 2) (+ r 1)))
          ((= direction dir-nul) (values (- f 1) (+ r 2)))))))
    (if (or (> prov-f 7) (< prov-f 0) (> prov-r 7) (< prov-r 0))
      #f
      (cls-to-coords (list prov-f prov-r)))))

(define cache:all-coords-in-direction
  ; indices:
  ; 0: coords-from
  ; 1: direction
  ; result: (coords-to ...)
  (list->vector
    (map
      (lambda (coords)
        (list->vector
          (map
            (lambda (direction)
              (let loop ((coords coords) (result '()))
                (let (
                    (next-coords
                      (calc-next-coords-in-direction coords direction)))
                  (if next-coords
                    (loop next-coords (cons next-coords result))
                    (reverse result)))))
            all-directions)))
      all-coords)))

(define (all-coords-in-direction coords direction)
  (vector-ref
    (vector-ref cache:all-coords-in-direction coords)
    direction))

(define (friendly-piece-at-coords? position coords color)
  (symbol=?
    (piece-color (piece-at-coords position coords))
    color))
      
(define (piece-at-coords? position coords)
  (not (= (piece-at-coords position coords) E)))

(define is-position-check?
  (memoized-proc position-check position-check-set!
    (lambda (position-in)
      (define position (position-copy-w-toggled-active-color position-in))
      (define king-to-capture
        (if (symbol=? (position-active-color position) 'w) k K)) 
      (define moves (legal-moves position #f))
      (let loop ((moves moves))
        (if (null? moves)
          #f
          (let ((move (car moves)))
            (if (= (piece-at-coords position (caddr move)) king-to-capture)
              #t
              (loop (cdr moves)))))))))

(define (is-position-checkmate? position)
  (and
    (null? (legal-moves position #f))
    (is-position-check? position)))

(define (is-position-stalemate? position)
  (and
    (null? (legal-moves position #f))
    (not (is-position-check? position))))

(define (legal-squares-for-rook position piece color coords)
  (legal-squares-along-directions
      piece color coords position rook-directions 7 #t #t))

(define (legal-squares-for-bishop position piece color coords)
  (legal-squares-along-directions
      piece color coords position bishop-directions 7 #t #t))

(define (legal-squares-for-queen position piece color coords)
  (legal-squares-along-directions
      piece color coords position queen-directions 7 #t #t))

(define (legal-squares-for-king position piece color coords)
  (legal-squares-along-directions
      piece color coords position king-directions 1 #t #t))

(define can-king-be-captured?
  (memoized-proc
    position-can-king-be-captured position-can-king-be-captured-set!
    (lambda (position)
      (define active-color (position-active-color position))
      (define king (if (symbol=? active-color 'w) k K))
      (let loop ((moves (legal-moves position #t)))
        (if (null? moves)
          #f
          (let ((move (car moves)))
            (if (= (piece-at-coords position (caddr move)) king)
              #t
              (loop (cdr moves)))))))))

(define (legal-squares-for-pawn position piece color coords)
  (define forward-direction (if (symbol=? color 'w) dir-u dir-d))
  (define forward-right-direction (if (symbol=? color 'w) dir-ur dir-dl))
  (define forward-left-direction (if (symbol=? color 'w) dir-ul dir-dr))
  (define initial-rank (if (symbol=? color 'w) 1 6))
  (append
    (legal-squares-along-directions
      piece color coords position
        (list forward-direction)
        (if (= (cadr (coords-to-cls coords)) initial-rank) 2 1) #f #t)
    (legal-squares-along-directions
      piece color coords position
      (list forward-right-direction forward-left-direction) 1 #t #f)))

(define (legal-square-for-knight-along-direction
                    dir position piece color coords)
  (let ((sqs-in-dir (all-coords-in-direction coords dir)))
    (if
      (or
        (null? sqs-in-dir)
        (friendly-piece-at-coords?
          position (car sqs-in-dir) color))
      #f
      (car sqs-in-dir))))

(define (legal-squares-for-knight position piece color coords)
  (fold-left
    (lambda (acc dir)
      (let ((sq (legal-square-for-knight-along-direction
                            dir position piece color coords)))
        (if sq (cons (list dir (list sq)) acc) acc)))
    '()
    knight-directions))

(define (legal-squares-from-coords position piece coords)
  (if (= piece E) '()
    (let ((m (modulo piece E)))
      (let (
          (proc
            (cond
              ((= m P-base) legal-squares-for-pawn)
              ((= m R-base) legal-squares-for-rook)
              ((= m B-base) legal-squares-for-bishop)
              ((= m N-base) legal-squares-for-knight)
              ((= m Q-base) legal-squares-for-queen)
              ((= m K-base) legal-squares-for-king))))
        (proc position piece (piece-color piece) coords)))))

(define (filter-out-moves-to-that-bring-king-in-check position moves)
  (filter
    (lambda (move)
      (not (can-king-be-captured? (position-after-move position move))))
    moves))

(define legal-moves
  (memoized-proc position-moves position-moves-set!
    (lambda (position allow-king-in-check)
      (let (
          (moves-w-king-possibly-in-check
            (legal-moves-w-king-possibly-in-check
              position)))
        (if allow-king-in-check
          moves-w-king-possibly-in-check
          (filter-out-moves-to-that-bring-king-in-check
                      position moves-w-king-possibly-in-check))))))

(define legal-squares-w-king-possibly-in-check
  (lambda (position)
    (let* (
        (active-color (position-active-color position))
        (white-to-play? (symbol=? active-color 'w))
        (black-to-play? (symbol=? active-color 'b)))
      (map-over-placement
        white-to-play?
        black-to-play?
        (lambda (piece coords-from)
          (list
            piece
            coords-from
            (legal-squares-from-coords
                position piece coords-from)))
        position))))

(define (legal-moves-w-king-possibly-in-check position)
  (let ((obj (legal-squares-w-king-possibly-in-check position)))
    (define moves '())
    (for-each-list-unpack (piece coords-from obj) obj
      (for-each-list-unpack (direction sqs) obj
        (for-each
          (lambda (coords-to)
            (define promotion-pieces
              (if
                (or
                  (and (= piece P) (= (cadr (coords-to-cls coords-to)) 7))
                  (and (= piece p) (= (cadr (coords-to-cls coords-to)) 0)))
                (if (symbol=? (position-active-color position) 'w)
                  (list R N B Q)
                  (list r n b q))
                '()))
            (if (null? promotion-pieces)
              (set! moves (cons (list piece coords-from coords-to '()) moves))
              (for-each
                (lambda (promotion-piece)
                  (set! moves
                    (cons
                      (list piece coords-from coords-to promotion-piece) moves)))
                promotion-pieces)))
          sqs)))
    moves))

(define (position-after-move position move)
  (list-unpack move (piece coords-from coords-to promotion-piece)
    (define capture? (piece-at-coords? position coords-to))
    (define pawn-move? (= (modulo piece E) 1))
    (define new-placement (bytevector-copy (position-placement position)))
    (bytevector-u8-set! new-placement coords-from E)
    (bytevector-u8-set! new-placement coords-to
      (if (null? promotion-piece) piece promotion-piece))
    (make-position
      new-placement
      (toggled-color (position-active-color position))
      (position-castling position)
      (position-en-passant position)
      (if (or capture? pawn-move?)
        0
        (1+ (position-halfmoves position)))
      (+
        (position-fullmoves position)
        (if (symbol=? (position-active-color position) 'b) 1 0))
      position
      move)))

(define (piece-base-value piece)
  (cond
    ((= piece E) 0)
    ((= piece P) +1)
    ((= piece p) -1)
    ((= piece N) +3)
    ((= piece n) -3)
    ((= piece B) +3)
    ((= piece b) -3)
    ((= piece Q) +9)
    ((= piece q) -9)
    ((= piece R) +5)
    ((= piece r) -5)
    ((= piece K) +999999)
    ((= piece k) -999999)))

(define evaluate-position-static
  (memoized-proc position-static-val position-static-val-set!
    (lambda (position)
      (define active-color (position-active-color position))
      (cond
        ((is-position-checkmate? position)
          (
            (if (symbol=? active-color 'w) - +)
            +inf.0))
        ((is-position-stalemate? position)
          0)
        (else
          (fold-left
            +
            0
            (map-over-placement
              #t #t
              (lambda (piece coords)
                (piece-base-value piece))
              position)))))))

; An evaluation object has the following structure:
; ((val move-seq) ...)

(define (less-predicate-for-eval-objs left-ls right-ls)
  (let* (
      (left-val (car left-ls))
      (left-move-seq (cadr left-ls))
      (right-val (car right-ls))
      (right-move-seq (cadr right-ls)))
    (cond
      ((and
        (infinite? left-val)
        (infinite? right-val)
        (= left-val right-val))
          (if (< left-val 0)
        ; When comparing two won sequences for black the
        ; best for black is the one where black wins in
        ; the fewest moves.
            (<
              (length left-move-seq)
              (length right-move-seq))
        ; When comparing two won sequences for white the
        ; best for black is the one where white wins in
        ; the most moves.
            (>
              (length left-move-seq)
              (length right-move-seq))))
      (else (< left-val right-val)))))

(define (display-val val)
  (cond
    ((infinite? val)
      (if (> val 0)
        (display "+#")
        (display "-#")))
    (else
      (begin
        (when (>= val 0)
          (display "+"))
        (display val)))))

(define (display-move-seq position move-seq)
  (let loop (
      (position position)
      (move-number 1)
      (move-seq move-seq)
      (first-iter #t))
    (define active-color (position-active-color position))
    (unless (null? move-seq)
      (when (or (symbol=? active-color 'w) first-iter)
        (display move-number)
        (display ".")
        (display " "))
      (when (symbol=? active-color 'b)
        (when first-iter
          (display "...")
          (display " ")))
      (display (move-to-alg position (car move-seq)))
      (display " ")
      (loop
        (position-after-move position (car move-seq))
        (if (symbol=? active-color 'b)
          (1+ move-number)
          move-number)
        (cdr move-seq)
        #f))))

(define (display-eval-obj position eval-obj)
  (let loop ((eval-obj eval-obj))
    (unless (null? eval-obj)
      (let ((val+move-seq (car eval-obj)))
        (let ((val (car val+move-seq)) (move-seq (cadr val+move-seq)))
          (display-val val)
          (display " ")
          (if (null? move-seq)
            (display "(no moves)")
            (display-move-seq position move-seq))
          (newline)
          (loop (cdr eval-obj)))))))

(define (legal-squares-along-direction
          piece color coords-from position direction
          max-distance allowed-to-capture? allowed-to-move-to-empty?)
  (let loop (
      (all-coords (all-coords-in-direction coords-from direction))
      (max-distance max-distance)
      (result '()))
    (if (or (= max-distance 0) (null? all-coords))
      result
      (let* (
          (coords (car all-coords))
          (piece-found (piece-at-coords position coords)))
        (if (= piece-found E)
          (if (not allowed-to-move-to-empty?) result
            (loop
              (cdr all-coords)
              (1- max-distance)
              (cons coords result)))
          (if
            (symbol=? (piece-color piece-found) color)
            result
            (if allowed-to-capture?
              (cons coords result)
              result)))))))

(define (legal-squares-along-directions
          piece color coords position directions
          max-distance allowed-to-capture? allowed-to-move-to-empty?)
  (map
    (lambda (direction)
      (list direction
        (legal-squares-along-direction
          piece color coords position direction
          max-distance allowed-to-capture? allowed-to-move-to-empty?)))
    directions))

(define (admit-disruptive position move)
  (let (
      (gain
        (- (evaluate-position-static (position-after-move position move))
           (evaluate-position-static position))))
    (if (symbol=? (position-active-color position) 'w)
      (>= gain 1.0)
      (<= gain -1.0))))

(define (sort-eval-obj position eval-obj)
  (let ((asc (sort less-predicate-for-eval-objs eval-obj)))
    (if (eq? (position-active-color position) 'w)
      (reverse asc) asc)))

(define (combine-move-w-eval-obj move eval-obj)
  ; This procedure takes a move and returns an eval-obj i.e. a
  ; list L of lists M with M = (evaluation move-seq-until-ply)
  ;
  ; (first) here picks the best continuation for the opponent.
  (let ((val+move-seq (first eval-obj)))
    (let* ((val (car val+move-seq)) (move-seq (cadr val+move-seq)))
      (list
        val
        (cons move move-seq)))))

(define (eval-obj-of-static-eval position)
  (list
    (list
      (evaluate-position-static position) ;eval
      '() ; move-seq
      )))

(define (evaluate-position-at-nonzero-ply position ply)
  (let (
      (all-moves (legal-moves position #f)))
    (if (null? all-moves)
      (eval-obj-of-static-eval position)
      (map
        (lambda (move)
          (let* (
              (new-pos (position-after-move position move))
              (eval-obj
                (evaluate-position-at-ply new-pos (- ply 0.5))))
            (combine-move-w-eval-obj move eval-obj)))
        all-moves))))

(define evaluate-position-at-ply
  (lambda (position ply)
    (sort-eval-obj position
      (if (= ply 0)
        (eval-obj-of-static-eval position)
        (evaluate-position-at-nonzero-ply position ply)))))

(define (display-position position)
  (let* ((enc (encode-fen position)))
    (display "position: ")
    (display enc)
    (newline)))

)
