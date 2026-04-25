(define d
  (lambda args
    (apply display args)
    (newline)))

(define-record-type Coords
  (fields
    rank
    file))

(define-record-type Piece
  (fields
    name
    color))

(define-record-type GameState
  (fields
    placement
    whose-turn))

(define-record-type Move
  (fields
    coords-from
    coords-to))

(define (fen->game-state fen)
  (define chars (string->list fen))
  (call/cc
    (lambda (cont)
      (fold-left
        (lambda (acc c)
          (cond
            [(eq? c #\/)
              (cont acc)]))
        '()
        chars))))
  

(define (main)
  (d "Hello!"))

(main)
