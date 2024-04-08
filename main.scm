(import (match match) (util))

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
(define position-index-moves 6)
(define position-index-moves-w-exp-k 7)
(define position-index-static-val 8)
(define position-index-check 9)

(define (position-placement p) (list-ref p position-index-placement))
(define (position-active-color p) (list-ref p position-index-active-color))
(define (position-castling p) (list-ref p position-index-castling))
(define (position-en-passant p) (list-ref p position-index-en-passant))
(define (position-halfmoves p) (list-ref p position-index-halfmoves))
(define (position-fullmoves p) (list-ref p position-index-fullmoves))
(define (position-moves p) (list-ref p position-index-moves))
(define (position-moves-w-exp-k p) (list-ref p position-index-moves-w-exp-k))
(define (position-static-val p) (list-ref p position-index-static-val))
(define (position-check? p) (list-ref p position-index-check))

(define white-pieces (list R N B Q K P))
(define black-pieces (list r n b q k p))
(define all-pieces (append white-pieces black-pieces))

(define knight-directions '(nur nru nrd ndr ndl nld nlu nul))

(define should-do-quiescence-search? #t)

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

(define (is-move-capture? placement move)
  (piece-at-coords? placement (cadr move)))

(define (move-to-alg position move)
  (define sq-from (car move))
  (define sq-to (cadr move))
  (define sq-to-str (square-to-alg sq-to))
  (define placement (position-placement position))
  (define piece-moving (piece-at-coords placement sq-from))
  (define capture? (is-move-capture? placement move))
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
            (when (and (not (= piece P)) (not (= piece p)))
              (when (= piece piece-moving)
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
            (when (and (not (= piece P)) (not (= piece p)))
              (when (= piece piece-moving)
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
      ((force (position-check? next-position)) "+")
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

(define (make-position placement active-color castling
                        en-passant halfmoves fullmoves)
  (define pos
    (list
      placement
      active-color
      castling
      en-passant
      halfmoves
      fullmoves
      (delay (available-moves-from-position pos))
      (delay (available-moves-from-position pos #f))
      (delay (evaluate-position-static pos))
      (delay (is-position-check? pos))))
  pos)

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
      (position-fullmoves position))))

(define (decode-fen fen-string)
  (let ((field-strings (string-split fen-string #\ )))
    (let ((placement (decode-placement-data (list-ref field-strings 0))))
      (make-position
        placement
        (decode-active-color (list-ref field-strings 1))
        (decode-castling (list-ref field-strings 2))
        (decode-en-passant (list-ref field-strings 3))
        (decode-halfmoves (list-ref field-strings 4))
        (decode-fullmoves (list-ref field-strings 5))))))

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

(define (is-position-check? position-in)
  (define position (position-copy-w-toggled-active-color position-in))
  (define placement (position-placement position))
  (define king-to-capture
    (if (symbol=? (position-active-color position) 'w) 14 6)) 
  (define moves (force (position-moves position)))
  (call/cc
    (lambda (cont)
      (for-each
        (lambda (move)
          (when
            (=
              (piece-at-coords placement (cadr move))
              king-to-capture)
            (cont #t)
            (exit)))
        moves)
      #f)))

(define (is-position-checkmate? position)
  (and
    (null? (force (position-moves position)))
    (force (position-check? position))))

(define (is-position-stalemate? position)
  (and
    (null? (force (position-moves position)))
    (not (force (position-check? position)))))

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
  (define placement (position-placement position))
  (define active-color (position-active-color position))
  (define king (if (symbol=? active-color 'w) k K))
  (define is-piece-of-active-color?
    (if (symbol=? active-color 'w) white-piece? black-piece?))
  (call/cc
    (lambda (cont)
      (for-each
        (lambda (move)
          (when (= (piece-at-coords placement (cadr move)) king)
            (cont #t)
            (exit)))
        (force (position-moves-w-exp-k position)))
      #f)))

(define (available-squares-for-pawn coords position)
  (define placement (position-placement position))
  (define color
    (if (member (piece-at-coords placement coords) white-pieces) 'w 'b))
  (define forward-direction (if (symbol=? color 'w) 'u 'd))
  (define forward-right-direction (if (symbol=? color 'w) 'ur 'dl))
  (define forward-left-direction (if (symbol=? color 'w) 'ul 'dr))
  (define initial-rank (if (symbol=? color 'w) 1 6))
  (append
    (available-squares-along-directions
      coords position
        (list forward-direction)
        (if (= (cadr coords) initial-rank) 2 1) #f #t)
    (available-squares-along-directions
      coords position
      (list forward-right-direction forward-left-direction) 1 #t #f)))

(define (available-squares-for-knight coords position)
  (define placement (position-placement position))
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
  (define placement (position-placement position))
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

(define available-moves-from-position
  (case-lambda
    ((position) (available-moves-from-position position #t))
    ((position dont-allow-exposed-king)
      (define placement (position-placement position))
      (define active-color (position-active-color position))
      (apply append
        (map-over-placement
          (lambda (piece coords-from)
            (map
              (lambda (coords-to)
                (list coords-from coords-to))
              (if
                (or
                  (and (symbol=? active-color 'w) (white-piece? piece))
                  (and (symbol=? active-color 'b) (black-piece? piece)))
                (available-squares-from-coords
                    coords-from position dont-allow-exposed-king)
                '())))
          placement)))))

(define (toggled-color color)
  (case color
    ((w) 'b)
    ((b) 'w)))

(define (position-after-move position move)
  (define placement (position-placement position))
  (define coords-from (car move))
  (define coords-to (cadr move))
  (define piece-moving (piece-at-coords placement coords-from))
  (define capture? (piece-at-coords? placement coords-to))
  (define pawn-move?
    (or
      (= piece-moving P)
      (= piece-moving p)))
  (define new-placement (bytevector-copy placement))
  (bytevector-u8-set! new-placement
    (placement-index (car coords-from) (cadr coords-from)) E)
  (bytevector-u8-set! new-placement
    (placement-index (car coords-to) (cadr coords-to)) piece-moving)
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
      (if (symbol=? (position-active-color position) 'b) 1 0))))

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
  (define active-color (position-active-color position))
  (cond
    ((is-position-checkmate? position)
      (
        (if (symbol=? active-color 'w) - +)
        +inf.0))
    ((is-position-stalemate? position)
      0)
    (else
      (let ((placement (position-placement position)))
        (fold-left
          +
          0
          (map-over-placement
            (lambda (piece coords)
              (piece-base-value piece))
            placement))))))

(define available-captures-from-position
  (case-lambda
    ((position) available-captures-from-position -1)
    ((position limit)
      (let loop (
          (result '())
          (len 0)
          (moves (force (position-moves position))))
        (if (or (= len limit) (null? moves)) result
          (let ((move (car moves)))
            (apply
              loop
              (let (
                  (coords-to (cadr move))
                  (placement (position-placement position))
                  (active-color (position-active-color position)))
                (if (enemy-piece-at-coords? placement coords-to active-color)
                  (list (cons move result) (1+ len) (cdr moves))
                  (list result len (cdr moves)))))))))))

(define (is-position-quiescent? position)
  (and
    (null?
      (available-captures-from-position position 1))
    (null?
      (available-captures-from-position
        (position-copy-w-toggled-active-color position) 1))
    ; If a position is check it means toggling the active color does have the
    ; "capture" of king but because the position is invalid it won't be found.
    ; It is not reasonable to treat checks as quiescent, therefore place this
    ; check but place it last for performance reasons.
    (not (force (position-check? position)))))

(define (evaluate-position-until-quiescence position)
  (if (is-position-quiescent? position)
    (evaluate-position-static position)
    ; Play a game Each player can either pass or play a move that has one or
    ; more of the following traits:
    ; * is a capture
    ; * is a check
    ; * is a checkmate
    ; * is a stalemate
    ; * any move (if in check)
    ; The game ends on checkmate, on stalemate or when both players have passed
    ; in which case the winner is decided by a static evaluation of the
    ; position.
    ; I should also be allowed to cover hanging pieces...
    ))

; An evaluation object has the following structure:
; ((val move-seq) ...)

(define (less-predicate-for-eval-objs left-ls right-ls)
  (match left-ls
    ((,left-val ,left-move-seq)
      (match right-ls
        ((,right-val ,right-move-seq)
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
            (else (< left-val right-val))))))))

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

(define (display-evaluation position eval-obj)
  (define active-color (position-active-color position))
  (let loop ((eval-obj eval-obj))
    (unless (null? eval-obj)
      (match (car eval-obj)
        ((,val ,move-seq)
          (display-val val)
          (display " ")
          (if (null? move-seq)
            (display "(no moves)")
            (display-move-seq position move-seq))
          (newline)
          (loop (cdr eval-obj)))))))

(define (available-squares-along-directions
          coords position directions
          max-distance capture-allowed? non-capture-allowed?)
  (define placement (position-placement position))
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

(define admit-all 'admit-all)

(define (admit-disruptive position move)
  (let (
      (gain
        (- (evaluate-position-static (position-after-move position move))
           (force (position-static-val position)))))
    (if (symbol=? (position-active-color position) 'w)
      (>= gain 1.0)
      (<= gain -1.0))))

(define (sort-eval-obj position eval-obj)
  (let ((asc (sort less-predicate-for-eval-objs eval-obj)))
    (if (eq? (position-active-color position) 'w)
      (reverse asc) asc)))

(define (evaluate-position-at-nonzero-ply position ply admissible-moves-pred)
  (let (
      (all-moves (force (position-moves position)))
      (pred (lambda (move) (admissible-moves-pred position move))))
    (let-values (
        ((admissible-moves inadmissible-moves)
          (if (eq? admissible-moves-pred admit-all)
            (values all-moves '())
            (partition pred all-moves))))
      (if (null? admissible-moves)
        (list
          (list (evaluate-position-static position) '()))
        (append
          (if (not (null? inadmissible-moves))
          ; Inadmissible moves exist but we will not explore them (after all,
          ; that's what being inadmissible means). Therefore include the static
          ; evaluation of the position which will represent all the
          ; inadmissible moves. If we did not include it then evaluation would
          ; explore lines where players would be forced to play admissible
          ; moves no matter how bad they are, ignoring the fact that
          ; inadmissible moves might well be better.
            (list
              (list (evaluate-position-static position) '()))
            '())
          (map
            (lambda (move)
              ; This procedure takes a move and returns an eval-obj i.e. a
              ; list L of lists M with M = (evaluation move-seq-until-ply)
              (define new-pos (position-after-move position move))
              (define eval-obj
                (evaluate-position-at-ply
                  new-pos (- ply 0.5) admissible-moves-pred))
              ; Pick only the best continuation for the opponent.
              (match (first eval-obj)
                ((,val ,move-seq)
                  (list val (cons move move-seq)))))
            admissible-moves))))))

(define evaluate-position-at-ply
  (case-lambda
    ((position ply)
      (evaluate-position-at-ply position ply admit-all))
    ((position ply admissible-moves-pred)
      (sort-eval-obj position
        (if (= ply 0)
          (if (eq? admissible-moves-pred admit-all)
            (evaluate-position-at-ply position 60/2 admit-disruptive)
            (list
              (list (evaluate-position-static position) '())))
          (evaluate-position-at-nonzero-ply
            position ply admissible-moves-pred))))))

(define (display-position position)
  (let* ((enc (encode-fen position)))
    (display "position: ")
    (display enc)
    (display " ")
    (display-move-seq-from-position position)
    (newline)))

(define fen-initial "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define fen-empty "8/8/8/8/8/8/8/8 w KQkq - 0 1")
(define fen-mate-in-2 "r1b2k1r/ppp1bppp/8/1B1Q4/5q2/2P5/PPP2PPP/R3R1K1 w - - 1 0")
(define fen-simple "6nk/8/8/8/8/8/8/KN6 w - - 0 1")

(define (main)
  (define position
    (decode-fen "k7/1b2n3/8/3q4/8/1Q3B2/8/K7 w - - 0 1"))

  (display-evaluation
    position
    (evaluate-position-at-ply position 0/2))

  (exit)

  )


(main)
