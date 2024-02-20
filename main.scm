(use-modules (ice-9 control))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (util))

(define white-pieces '(R N B Q K P))
(define black-pieces '(r n b q k p))
(define pieces (append white-pieces black-pieces))

(define knight-directions '(nur nru nrd ndr ndl nld nlu nul))

(define-public (memoized-proc proc . args)
  (define max-cache-size 50000)
  (define key-to-cache-sublist (make-hash-table))
  (define cache '())
  (define cache-size 0)
  (define cache-last-pair '())
  (lambda args
    (define cache-sublist (hash-ref key-to-cache-sublist args))
    (if (eq? cache-sublist #f)
      (let* (
          (proc-result (apply proc args))
          (cache-entry (cons args proc-result)))
        ; cache grows from the end so we can easily drop the oldest records
        ; from the front
        (if (null? cache)
          (begin
            (set! cache (list cache-entry))
            (set! cache-last-pair cache))
          (begin
            (set-cdr! cache-last-pair (list cache-entry))
            (set! cache-last-pair (cdr cache-last-pair))))
        (set! cache-size (1+ cache-size))
        (when (> cache-size max-cache-size)
          (let ((args-to-forget (caar cache)))
            (set! cache (cdr cache))
            (hash-remove! key-to-cache-sublist args-to-forget)
            (set! cache-size (1- cache-size))))
        (hash-set! key-to-cache-sublist args cache-last-pair)
        (cdr cache-entry))
      (let ((cache-entry (car cache-sublist))) (cdr cache-entry)))))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (name . args) expr expr* ...)
      (define name
        (memoized-proc
          (lambda args expr expr* ...))))))

(define-syntax define-public-memoized
  (syntax-rules ()
    ((_ (name . args) expr expr* ...)
     (begin
       (define-memoized (name . args) expr expr* ...)
       (export name)))))

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
        (file-to-alg (car sq))
        (rank-to-alg (cadr sq))))

(define (move-to-alg position move)
    (define sq-from (car move))
    (define sq-to (cadr move))
    (define sq-to-str (square-to-alg sq-to))
    (define placement (list-ref position 0))
    (define piece-moving (piece-at-coords placement sq-from))
    (define capture? (piece-at-coords? placement sq-to))
    (define capture-str (if capture? "x" ""))
    (define piece-moving-str
        (case piece-moving
            ((P p)
                (if capture?
                    (file-to-alg (car sq-from))
                    ""))
            ((N n) "N")
            ((B b) "B")
            ((Q q) "Q")
            ((R r) "R")
            ((K k) "K")))
    (define rank-str
        (call/ec
            (lambda (return)
                (for-each
                    (lambda (piece f r)
                        (when (and (not (eq? piece 'P)) (not (eq? piece 'p)))
                            (when (eq? piece piece-moving)
                                (when (= f (car sq-from))
                                    ; When same type of piece on same file.
                                    (when (not (equal? (list f r) sq-from))
                                        (for-each
                                            (lambda (sq)
                                                (when (equal? sq sq-to)
                                                    (return
                                                        (rank-to-alg
                                                            (cadr sq-from)))))
                                            (available-squares-from-coords
                                                 (list f r) position #t)))))))
                    placement
                    file-coords
                    rank-coords)
                "")))
    (define file-str
        (call/ec
            (lambda (return)
                (for-each
                    (lambda (piece f r)
                        (when (and (not (eq? piece 'P)) (not (eq? piece 'p)))
                            (when (eq? piece piece-moving)
                                (when (not (= f (car sq-from)))
                                    ; When same type of piece on different file.
                                    (when (not (equal? (list f r) sq-from))
                                        (for-each
                                            (lambda (sq)
                                                (when (equal? sq sq-to)
                                                    (return
                                                        (file-to-alg
                                                            (car sq-from)))))
                                            (available-squares-from-coords
                                                 (list f r) position #t)))))))
                    placement
                    file-coords
                    rank-coords)
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
            check-or-checkmate-str))
    result)

(define (white-piece? piece)
    (member piece white-pieces))

(define (black-piece? piece)
    (member piece black-pieces))

(define file-coords
    (apply append (make-list 8 (list 0 1 2 3 4 5 6 7))))
(define rank-coords
    (apply append (map (lambda (f) (make-list 8 f)) (list 0 1 2 3 4 5 6 7))))

(define (decode-rank rank-string)
    (reverse
        (fold
            (lambda (char current-result)
                (case char
                    ((#\r #\n #\b #\q #\k #\p #\R #\N #\B #\Q #\K #\P)
                        (cons
                            (string->symbol (string char))
                            current-result))
                    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8)
                        (append
                            (make-list (string->number (string char)) '())
                            current-result))))
            '()
            (string->list rank-string))))

(define (piece-at-coords placement coords)
    (let ((f (car coords)) (r (cadr coords)))
        (list-ref placement (+ (* r 8) f))))

(define (decode-placement-data placement-data-string)
    (apply
        append
        (reverse
            (let ((rank-strings (string-split placement-data-string #\/)))
                (map decode-rank rank-strings)))))

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

(define (decode-fen fen-string)
    (let ((field-strings (string-split fen-string #\ )))
        (list
            (decode-placement-data (list-ref field-strings 0))
            (decode-active-color (list-ref field-strings 1))
            (decode-castling (list-ref field-strings 2))
            (decode-en-passant (list-ref field-strings 3))
            (decode-halfmoves (list-ref field-strings 4))
            (decode-fullmoves (list-ref field-strings 5)))))

(define (next-coords-in-direction coords direction)
    (let* (
            (f (car coords)) (r (cadr coords))
            (provisional
                (case direction
                    ((u) (list f (1+ r)))
                    ((r) (list (1+ f) r))
                    ((d) (list f (1- r)))
                    ((l) (list (1- f) r))
                    ((ur) (list (1+ f) (1+ r)))
                    ((dr) (list (1+ f) (1- r)))
                    ((dl) (list (1- f) (1- r)))
                    ((ul) (list (1- f) (1+ r)))
                    ; Knight directions: The first character gives the
                    ; two-square movement and the second character the
                    ; one-square move. For example, 'ndr means "knight move,
                    ; first two squares down then one square right".
                    ((nur) (list (+ f 1) (+ r 2)))
                    ((nru) (list (+ f 2) (+ r 1)))
                    ((nrd) (list (+ f 2) (- r 1)))
                    ((ndr) (list (+ f 1) (- r 2)))
                    ((ndl) (list (- f 1) (- r 2)))
                    ((nld) (list (- f 2) (- r 1)))
                    ((nlu) (list (- f 2) (+ r 1)))
                    ((nul) (list (- f 1) (+ r 2)))))
            (prov-f (car provisional))
            (prov-r (cadr provisional))
            (result
                (if (or (> prov-f 7) (< prov-f 0) (> prov-r 7) (< prov-r 0))
                    '()
                    (list prov-f prov-r))))
        result))

(define (friendly-piece-at-coords? placement coords color)
    (member
        (piece-at-coords placement coords)
        (case color
            ((w) white-pieces)
            ((b) black-pieces))))
            
(define (enemy-piece-at-coords? placement coords color)
    (member
        (piece-at-coords placement coords)
        (case color
            ((w) black-pieces)
            ((b) white-pieces))))

(define (piece-at-coords? placement coords)
    (not (null? (piece-at-coords placement coords))))

(define (toggle-active-color position)
    (define active-color (list-ref position 1))
    (define copy (list-copy position))
    (list-set! copy 1
        (if (eq? active-color 'w) 'b 'w))
    copy)

(define (is-position-check? position-in)
    (define position (toggle-active-color position-in))
    (define placement (list-ref position 0))
    (define king-to-capture (if (eq? (list-ref position 1) 'w) 'k 'K)) 
    (define moves (available-moves-from-position position #t))
    (call/ec
        (lambda (return)
            (for-each
                (lambda (move)
                    (when
                        (eq?
                            (piece-at-coords placement (cadr move))
                            king-to-capture)
                        (return #t)))
                moves)
            #f)))

(define (is-position-checkmate? position)
    (and
        (null? (available-moves-from-position position #t))
        (is-position-check? position)))

(define (is-position-stalemate? position)
    (and
        (null? (available-moves-from-position position #t))
        (not (is-position-check? position))))

(define (filter-out-moves-that-allow-king-to-be-captured position moves)
    (filter
        (lambda (move)
            (not (can-king-be-captured (position-after-move position move))))
        moves))

(define (available-squares-for-rook coords position)
    (available-squares-along-directions
            coords position '(u r d l) 7 #t #t))

(define (available-squares-for-bishop coords position)
    (available-squares-along-directions
            coords position '(ur dr dl ul) 7 #t #t))

(define (available-squares-for-queen coords position)
    (available-squares-along-directions
            coords position '(u ur r dr d dl l ul) 7 #t #t))

(define (available-squares-for-king coords position)
    (available-squares-along-directions
            coords position '(u ur r dr d dl l ul) 1 #t #t))

(define (can-king-be-captured position)
    (define placement (list-ref position 0))
    (define active-color (list-ref position 1))
    (define king (if (eq? active-color 'w) 'k 'K))
    (define is-piece-of-active-color?
        (if (eq? active-color 'w) white-piece? black-piece?))
    (call/ec
        (lambda (return)
            (for-each
                (lambda (move)
                    (when (eq? (piece-at-coords placement (cadr move)) king)
                        (return #t)))
                (available-moves-from-position position #f))
            #f)))

(define (available-squares-for-pawn coords position)
    (define placement (list-ref position 0))
    (define color
        (if (member (piece-at-coords placement coords) white-pieces) 'w 'b))
    (define forward-direction (if (eq? color 'w) 'u 'd))
    (define forward-right-direction (if (eq? color 'w) 'ur 'dl))
    (define forward-left-direction (if (eq? color 'w) 'ul 'dr))
    (define initial-rank (if (eq? color 'w) 1 6))
    (append
        (available-squares-along-directions
            coords position
                (list forward-direction)
                (if (= (cadr coords) initial-rank) 2 1) #f #t)
        (available-squares-along-directions
            coords position
            (list forward-right-direction forward-left-direction) 1 #t #f)))

(define (available-squares-for-knight coords position)
    (define placement (list-ref position 0))
    (define
        color
        (if (member (piece-at-coords placement coords) white-pieces)
            'w 'b))
    (filter
        (lambda (candidate-coords)
            (and (not (null? candidate-coords))
                (not
                    (friendly-piece-at-coords?
                        placement candidate-coords color))))
        (map
            (lambda (direction)
                (next-coords-in-direction coords direction))
            knight-directions)))

(define (available-squares-from-coords coords position check-for-checks)
    (define placement (list-ref position 0))
    (define unchecked-for-checks
        (case (piece-at-coords placement coords)
            ((()) '())
            ((P p) (available-squares-for-pawn coords position))
            ((N n) (available-squares-for-knight coords position))
            ((B b) (available-squares-for-bishop coords position))
            ((Q q) (available-squares-for-queen coords position))
            ((R r) (available-squares-for-rook coords position))
            ((K k) (available-squares-for-king coords position))))
    (if check-for-checks
        (map
            cadr
            (filter-out-moves-that-allow-king-to-be-captured
                position
                (map
                    (lambda (sq)
                        (list coords sq))
                    unchecked-for-checks)))
        unchecked-for-checks))

(define (available-moves-from-position position check-for-checks)
    (define placement (list-ref position 0))
    (define active-color (list-ref position 1))
    (fold
        (lambda (piece f r previous)
            (define coords-from (list f r))
            (append
                (map
                    (lambda (coords-to)
                        (list coords-from coords-to))
                    (if
                        (or
                            (and (eq? active-color 'w) (white-piece? piece))
                            (and (eq? active-color 'b) (black-piece? piece)))
                        (available-squares-from-coords
                            coords-from position check-for-checks)
                        '()))
                previous))
        '()
        placement
        file-coords
        rank-coords))

(define (toggled-color color)
    (case color
        ((w) 'b)
        ((b) 'w)))

(define (position-after-move position move)
    (define placement (list-ref position 0))
    (define coords-from (car move))
    (define coords-to (cadr move))
    (define piece-moving (piece-at-coords placement coords-from))
    (define capture? (piece-at-coords? placement coords-to))
    (define pawn-move?
        (or
            (eq? piece-moving 'P)
            (eq? piece-moving 'p)))
    (define new-placement
        (map
            (lambda (piece f r)
                (define coords (list f r))
                (cond
                    ((equal? coords coords-from)
                        '())
                    ((equal? coords coords-to)
                        piece-moving)
                    (else piece)))
            placement
            file-coords
            rank-coords))
    (list
        new-placement
        (toggled-color (list-ref position 1))
        (list-ref position 2)
        (list-ref position 3)
        (if (or capture? pawn-move?)
            0
            (1+ (list-ref position 4)))
        (+
            (list-ref position 5)
            (if (eq? (list-ref position 1) 'b) 1 0))))

(define (piece-base-value piece)
    (case piece
        ((()) 0)
        ((P) +1)
        ((p) -1)
        ((N) +3)
        ((n) -3)
        ((B) +3)
        ((b) -3)
        ((Q) +9)
        ((q) -9)
        ((R) +5)
        ((r) -5)
        ((K) +999999)
        ((k) -999999)))

(define (evaluate-position-static position)
    (define active-color (list-ref position 1))
    (cond
        ((is-position-checkmate? position)
            (
                (if (eq? active-color 'w) + -)
                (inf)))
        ((is-position-stalemate? position)
            0)
        (else
            (let ((placement (list-ref position 0)))
                (sum (map piece-base-value placement))))))

; An evaluation object has the following structure:
; ((val move-seq) ...)

(define (evaluate-position-at-ply position ply)
    (if (= ply 0)
        (list
            (list (evaluate-position-static position) '()))
        (let ((unsorted
                (map
                    (lambda (move)
                        (define new-pos (position-after-move position move))
                        (define eval-obj
                            (evaluate-position-at-ply new-pos (- ply 0.5)))
                        (if (null? eval-obj)
                            (raise 'todo)
                        ; Pick only the best continuation for the opponent.
                            (let ((sel-proc
                                    (if (eq? (list-ref position 1) 'w)
                                        first last)))
                                (match (sel-proc eval-obj)
                                    ((val move-seq)
                                        (list val (cons move move-seq)))))))
                    (available-moves-from-position position #t))))
            (sort
                unsorted
                (lambda (left-ls right-ls)
                    (<
                        (list-ref left-ls 0)
                        (list-ref right-ls 0)))))))

(define (display-val val)
    (when (>= val 0)
        (display "+"))
    (display val))

(define (display-move-seq position move-seq)
    (let loop (
            (position position)
            (move-number 1)
            (move-seq move-seq)
            (first #t))
        (define active-color (list-ref position 1))
        (unless (null? move-seq)
            (when (or (eq? active-color 'w) first)
                (display move-number)
                (display ".")
                (display " "))
            (when (eq? active-color 'b)
                (when first
                    (display "...")
                    (display " ")))
            (display (move-to-alg position (car move-seq)))
            (display " ")
            (loop
                (position-after-move position (car move-seq))
                (if (eq? active-color 'b)
                    (1+ move-number)
                    move-number)
                (cdr move-seq)
                #f))))

(define (display-evaluation position eval-obj)
    (define active-color (list-ref position 1))
    (define sorted-according-to-active-color
        (if (eq? active-color 'w) (reverse eval-obj) eval-obj))
    (match sorted-according-to-active-color
        (((val* move-seq*) ...)
            (for-each
                (lambda (val move-seq)
                    (display-val val)
                    (display " ")
                    (display-move-seq position move-seq)
                    (newline))
                val*
                move-seq*))))

(define (available-squares-along-directions
            coords position directions
            max-distance capture-allowed? non-capture-allowed?)
    (define placement (list-ref position 0))
    (define
        color
        (if (member (piece-at-coords placement coords) white-pieces)
            'w 'b))
    (fold
        (lambda (direction previous)
            (append previous
                (let loop (
                        (coords (next-coords-in-direction coords direction))
                        (max-distance max-distance)
                        (list-of-coords-out '()))
                    (cond
                        ((= max-distance 0)
                            list-of-coords-out)
                        ((null? coords)
                            list-of-coords-out)
                        ((friendly-piece-at-coords? placement coords color)
                            list-of-coords-out)
                        ((enemy-piece-at-coords? placement coords color)
                            (if capture-allowed?
                                (cons coords list-of-coords-out)
                                list-of-coords-out))
                        ((not non-capture-allowed?)
                            list-of-coords-out)
                        (else (loop
                                (next-coords-in-direction coords direction)
                                (1- max-distance)
                                (cons coords list-of-coords-out)))))))
        '()
        directions))

(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")

(define (main)
    ; Solution is:      1. Qb8+ Nxb8 2. Rd8#
    (define position
        (decode-fen
         "4kb2/3n1p2/8/6B1/8/1Q6/8/2KR4 w - - 1 1"))

    (display-evaluation
        position
        (evaluate-position-at-ply
            position
            1.0))
)

(main)
