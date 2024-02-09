(use-modules (srfi srfi-1))
(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (util))

(define white-pieces '(R N B Q K P))
(define black-pieces '(r n b q k p))
(define pieces (append white-pieces black-pieces))

(define knight-directions '(nur nru nrd ndr ndl nld nlu nul))

(define (char->symbol char)
    (string->symbol (string char)))

(define (decode-algebraic-square algebraic-square-string)
    (let* (
            (chars (string->list algebraic-square-string))
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
        (else (decode-algebraic-square en-passant-string))))

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

(define (available-squares-from-coords coords position)
    (define placement (list-ref position 0))
    (case (piece-at-coords placement coords)
        ((()) '())
        ((P p) (available-squares-for-pawn coords position))
        ((N n) (available-squares-for-knight coords position))
        ((B b) (available-squares-for-bishop coords position))
        ((Q q) (available-squares-for-queen coords position))
        ((R r) (available-squares-for-rook coords position))
        ((K k) (available-squares-for-king coords position))))

(define (available-moves-from-position position)
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
                        (available-squares-from-coords coords-from position)
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
    (define position1 (decode-fen fen-initial))
    (define move1 '((4 1) (4 3)))
    (define position2 (position-after-move position1 move1))
    (d position2))

(main)
