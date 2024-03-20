(import (util))

(define positions-that-were-expanded-for-moves (make-eq-hashtable))

(define profile? #f)
(define caching? #t)

(define E 0)
(define R 2)
(define N 3)
(define B 4)
(define Q 5)
(define K 6)
(define P 1)
(define r 10)
(define n 11)
(define b 12)
(define q 13)
(define k 14)
(define p 9)

(define position-index-placement 0)
(define position-index-active-color 1)
(define position-index-castling 2)
(define position-index-en-passant 3)
(define position-index-halfmoves 4)
(define position-index-fullmoves 5)
(define position-index-last-position 6)
(define position-index-last-move 7)

(define white-pieces (list R N B Q K P))
(define black-pieces (list r n b q k p))
(define all-pieces (append white-pieces black-pieces))

(define knight-directions '(nur nru nrd ndr ndl nld nlu nul))

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

(define (memoized-proc hash-function equiv proc)
  (define cache (make-hashtable hash-function equiv))
  (lambda args
    (define key args)
    (define cached-result (hashtable-ref cache key 'cache:not-exists))
    (if (or (not caching?) (eq? cached-result 'cache:not-exists))
      (let ((result (apply proc args)))
        (hashtable-set! cache key result)
        result)
      cached-result)))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ hash-function equiv (name . args) expr expr* ...)
      (define name
        (memoized-proc hash-function equiv (lambda args expr expr* ...))))))

(define placement-indices
  (let loop ((f 0) (r 0) (result '()))
    (if (> r 7)
      result
      (loop
        (if (= f 7)
          0
          (1+ f))
        (if (= f 7)
          (1+ r)
          r)
        (cons (list f r) result)))))

(define (placement-index f r)
  (+ (* 8 r) f))

(define (placement-ref placement f r)
  (bytevector-u8-ref placement (placement-index f r)))

(define (map-over-placement proc placement)
  (map
    (lambda (coords)
      (proc (placement-ref placement (car coords) (cadr coords)) coords))
    placement-indices))

(define (for-each-over-placement proc placement)
  (for-each
    (lambda (coords)
      (proc (placement-ref placement (car coords) (cadr coords)) coords))
    placement-indices))

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
  (define placement (list-ref position position-index-placement))
  (define piece-moving (piece-at-coords placement sq-from))
  (define capture? (piece-at-coords? placement sq-to))
  (define capture-str (if capture? "x" ""))
  (define piece-moving-str
    (cond
      ((or (= piece-moving P) (= piece-moving p))
        (if capture?
          (file-to-alg (car sq-from))
          ""))
      ((or (= piece-moving N) (= piece-moving n)) "N")
      ((or (= piece-moving B) (= piece-moving b)) "B")
      ((or (= piece-moving Q) (= piece-moving q)) "Q")
      ((or (= piece-moving R) (= piece-moving r)) "R")
      ((or (= piece-moving K) (= piece-moving k)) "K")
      (else (debug))))
  (define rank-str
    (call/cc
      (lambda (cont)
        (for-each-over-placement
          (lambda (piece coords)
            (when (and (not (eq? piece 'P)) (not (eq? piece 'p)))
              (when (eq? piece piece-moving)
                (when (= (car coords) (car sq-from))
                  ; When same type of piece on same file.
                  (when (not (equal? coords sq-from))
                    (for-each
                      (lambda (sq)
                        (when (equal? sq sq-to)
                          (cont
                            (rank-to-alg
                              (cadr sq-from)))
                          (exit)))
                      (available-squares-from-coords
                         coords position #t)))))))
          placement)
        "")))
  (define file-str
    (call/cc
      (lambda (cont)
        (for-each-over-placement
          (lambda (piece coords)
            (when (and (not (eq? piece 'P)) (not (eq? piece 'p)))
              (when (eq? piece piece-moving)
                (when (not (= (car coords) (car sq-from)))
                  ; When same type of piece on different file.
                  (when (not (equal? coords sq-from))
                    (for-each
                      (lambda (sq)
                        (when (equal? sq sq-to)
                          (cont
                            (file-to-alg
                              (car sq-from)))
                          (exit)))
                      (available-squares-from-coords
                         coords position #t)))))))
          placement)
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

(define (piece-at-coords placement coords)
  (placement-ref placement (car coords) (cadr coords)))

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

(define (decode-fen fen-string)
  (let ((field-strings (string-split fen-string #\ )))
    (let ((placement (decode-placement-data (list-ref field-strings 0))))
      (list
        placement
        (decode-active-color (list-ref field-strings 1))
        (decode-castling (list-ref field-strings 2))
        (decode-en-passant (list-ref field-strings 3))
        (decode-halfmoves (list-ref field-strings 4))
        (decode-fullmoves (list-ref field-strings 5))
        '() ; last position
        '() ; last move
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
  (define placement (list-ref position position-index-placement))
  (define active-color (list-ref position position-index-active-color))
  (define castling (list-ref position position-index-castling))
  (define en-passant (list-ref position position-index-en-passant))
  (define halfmoves (list-ref position position-index-halfmoves))
  (define fullmoves (list-ref position position-index-fullmoves))
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
  (not (= (piece-at-coords placement coords) E)))

(define (toggle-active-color position)
  (define active-color (list-ref position position-index-active-color))
  (define new-position (list-copy position))
  (list-set! new-position 1 (if (eq? active-color 'w) 'b 'w))
  new-position)

(define (is-position-check? position-in)
  (define position (toggle-active-color position-in))
  (define placement (list-ref position position-index-placement))
  (define king-to-capture (if (eq? (list-ref position position-index-active-color) 'w) 'k 'K)) 
  (define moves (available-moves-from-position position #t))
  (call/cc
    (lambda (cont)
      (for-each
        (lambda (move)
          (when
            (eq?
              (piece-at-coords placement (cadr move))
              king-to-capture)
            (cont #t)
            (exit)))
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

(define (can-king-be-captured? position)
  (define placement (list-ref position position-index-placement))
  (define active-color (list-ref position position-index-active-color))
  (define king (if (eq? active-color 'w) k K))
  (define is-piece-of-active-color?
    (if (eq? active-color 'w) white-piece? black-piece?))
  (call/cc
    (lambda (cont)
      (for-each
        (lambda (move)
          (when (= (piece-at-coords placement (cadr move)) king)
            (cont #t)
            (exit)))
        (available-moves-from-position position #f))
      #f)))

(define (available-squares-for-pawn coords position)
  (define placement (list-ref position position-index-placement))
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
  (define placement (list-ref position position-index-placement))
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

(define (available-squares-from-coords coords position dont-allow-exposed-king)
  (define placement (list-ref position position-index-placement))
  (define unchecked-for-checks
    (let ((piece (piece-at-coords placement coords)))
      (cond
        ((= piece E) '())
        ((or (= piece P) (= piece p))
          (available-squares-for-pawn coords position))
        ((or (= piece N) (= piece n))
          (available-squares-for-knight coords position))
        ((or (= piece B) (= piece b))
          (available-squares-for-bishop coords position))
        ((or (= piece Q) (= piece q))
          (available-squares-for-queen coords position))
        ((or (= piece R) (= piece r))
          (available-squares-for-rook coords position))
        ((or (= piece K) (= piece k))
          (available-squares-for-king coords position)))))
  (if dont-allow-exposed-king
    (map
      cadr
      (filter
        (lambda (move)
          (not (can-king-be-captured?
              (position-after-move position move))))
        (map
          (lambda (sq)
            (list coords sq))
          unchecked-for-checks)))
    unchecked-for-checks))

(define (display-move-seq-from-position position)
  (match
    (let loop ((position position) (move-seq '()))
      (define last-position (list-ref position position-index-last-position))
      (define last-move (list-ref position position-index-last-move))
      (if (null? last-position)
        (list position move-seq)
        (loop last-position (cons last-move move-seq))))
    ((position move-seq)
      (display-move-seq position move-seq))))

(define-memoized
    (lambda (args)
      (let ((position (car args)) (dont-allow-exposed-king (cadr args)))
        (string-hash
          (string-append
            (encode-fen position)
            (if dont-allow-exposed-king "t" "f")))))
    (lambda (left-args right-args)
      (let* (
          (position-left (car left-args))
          (dont-allow-exposed-king-left (cadr left-args))
          (position-right (car right-args))
          (dont-allow-exposed-king-right (cadr right-args)))
        (and
          (eq? dont-allow-exposed-king-left dont-allow-exposed-king-right)
          (string=?
            (encode-fen position-left)
            (encode-fen position-right)))))
    (available-moves-from-position position dont-allow-exposed-king)
  (define placement (list-ref position position-index-placement))
  (define active-color (list-ref position position-index-active-color))
  (apply append
    (map-over-placement
      (lambda (piece coords-from)
        (map
          (lambda (coords-to)
            (list coords-from coords-to))
          (if
            (or
              (and (eq? active-color 'w) (white-piece? piece))
              (and (eq? active-color 'b) (black-piece? piece)))
            (available-squares-from-coords
                coords-from position dont-allow-exposed-king)
            '())))
      placement)))

(define (toggled-color color)
  (case color
    ((w) 'b)
    ((b) 'w)))

(define (position-after-move position move)
  (define placement (list-ref position position-index-placement))
  (define coords-from (car move))
  (define coords-to (cadr move))
  (define piece-moving (piece-at-coords placement coords-from))
  (define capture? (piece-at-coords? placement coords-to))
  (define pawn-move?
    (or
      (eq? piece-moving 'P)
      (eq? piece-moving 'p)))
  (define new-placement (bytevector-copy placement))
  (bytevector-u8-set! new-placement
    (placement-index (car coords-from) (cadr coords-from)) E)
  (bytevector-u8-set! new-placement
    (placement-index (car coords-to) (cadr coords-to)) piece-moving)
  (list
    new-placement
    (toggled-color (list-ref position position-index-active-color))
    (list-ref position position-index-castling)
    (list-ref position position-index-en-passant)
    (if (or capture? pawn-move?)
      0
      (1+ (list-ref position position-index-halfmoves)))
    (+
      (list-ref position position-index-fullmoves)
      (if (eq? (list-ref position position-index-active-color) 'b) 1 0))
    position
    move))

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

(define (evaluate-position-static position)
  (define active-color (list-ref position position-index-active-color))
  (cond
    ((is-position-checkmate? position)
      (
        (if (eq? active-color 'w) - +)
        (inf)))
    ((is-position-stalemate? position)
      0)
    (else
      (let ((placement (list-ref position position-index-placement)))
        (fold-left
          +
          0
          (map-over-placement
            (lambda (piece coords)
              (piece-base-value piece))
            placement))))))

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
              (list
                (evaluate-position-static new-pos)
                (cons move '()))
            ; Pick only the best continuation for the opponent.
              (let ((sel-proc
                  (if (eq? (list-ref position position-index-active-color) 'w)
                    first last)))
                (let* (
                    (val-move-seq (sel-proc eval-obj))
                    (val (car val-move-seq))
                    (move-seq (cadr val-move-seq)))
                  (list val (cons move move-seq))))))
          (available-moves-from-position position #t))))
      (sort
        (lambda (left-ls right-ls)
          (let ((left-val (car left-ls)) (right-val (car right-ls)))
            (cond
              ((and (infinite? left-val) (infinite? right-val))
                (if (< left-val 0)
              ; When comparing two won sequences for black the
              ; best for black is the one where black wins in
              ; the fewest moves.
                  (<
                    (length (cadr left-ls))
                    (length (cadr right-ls)))
              ; When comparing two won sequences for white the
              ; best for black is the one where white wins in
              ; the most moves.
                  (>
                    (length (cadr left-ls))
                    (length (cadr right-ls)))))
              (else (< left-val right-val)))))
        unsorted))))

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
    (define active-color (list-ref position position-index-active-color))
    (unless (null? move-seq)
      (when (or (eq? active-color 'w) first-iter)
        (display move-number)
        (display ".")
        (display " "))
      (when (eq? active-color 'b)
        (when first-iter
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
  (define active-color (list-ref position position-index-active-color))
  (define sorted-according-to-active-color
    (if (eq? active-color 'w) (reverse eval-obj) eval-obj))
  (let loop ((ls sorted-according-to-active-color))
    (unless (null? ls)
      (let ((val-move-seq (car ls)))
        (let ((val (car val-move-seq)) (move-seq (cadr val-move-seq)))
          (display-val val)
          (display " ")
          (display-move-seq position move-seq)
          (newline)
          (loop (cdr ls)))))))

(define (available-squares-along-directions
          coords position directions
          max-distance capture-allowed? non-capture-allowed?)
  (define placement (list-ref position position-index-placement))
  (define
    color
    (if (member (piece-at-coords placement coords) white-pieces)
      'w 'b))
  (apply append
    (map
      (lambda (direction)
        (let loop (
            (coords (next-coords-in-direction coords direction))
            (max-distance max-distance)
            (result '()))
          (cond
            ((= max-distance 0)
              result)
            ((null? coords)
              result)
            ((friendly-piece-at-coords? placement coords color)
              result)
            ((enemy-piece-at-coords? placement coords color)
              (if capture-allowed?
                (cons coords result)
                result))
            ((not non-capture-allowed?)
              result)
            (else
              (loop
                (next-coords-in-direction coords direction)
                (1- max-distance)
                (cons coords result))))))
      directions)))

(define (display-position position)
  (let* ((enc (encode-fen position)))
    (display "position: ")
    (display enc)
    (display " ")
    (display-move-seq-from-position position)
    (newline)))


(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")
(define fen-mate-in-2 "4kb1r/p2n1ppp/4q3/4p1B1/4P3/1Q6/PPP2PPP/2KR4 w k - 1 0")
(define simple-position "6nk/8/8/8/8/8/8/KN6 w - - 0 1")

(define (main)
  (define position
    (decode-fen simple-position))

  (display-evaluation
    position
   (evaluate-position-at-ply
     position
     5/2)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;(for-each
  ;  display-position
  ;  (hash-map->list
  ;    (lambda (position _) position)
  ;    positions-that-were-expanded-for-moves))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;(d (hash-count (lambda (k v) #t) positions-that-were-expanded-for-moves))

  )

;(define (main)
;  (define position (decode-fen fen-initial))
;  (define loops 10000)
;  (define t0 (seconds-since-epoch))
;  (define t1 -1)
;  (let outer-loop ((i loops))
;    (when (> i 0)
;      (available-moves-from-position position #t)
;      ;(for-each d (available-squares-for-knight (list 0 1) position))
;      ;(d (next-coords-in-direction (list 1 0) 'nul))
;      (outer-loop (1- i))))
;  (set! t1 (seconds-since-epoch))
;  (display (* 1000.0 (/ (- t1 t0) loops)))
;  (display " ms per loop, ")
;  (display loops)
;  (display " loops")
;  (newline)
;)

(main)
