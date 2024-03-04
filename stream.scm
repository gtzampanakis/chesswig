(define-module (stream))

; A stream is either:
; * A pair whose
;       * car is a thunk that returns a value and whose
;       * cdr is a thunk that returns a stream or
; * stream-null

(use-modules (util))

(define-public stream-null '())

(define-public stream-null? null?)

(define-public (stream-for-each proc s)
    (let loop ((s s))
        (unless (stream-null? s)
            (proc ((car s)))
            (loop ((cdr s))))))

(define-public (stream-map proc s)
    (let loop ((s s))
        (if (stream-null? s)
            stream-null
            (cons
                (lambda () (proc ((car s))))
                (lambda () (loop ((cdr s))))))))

(define-public stream-append 
    (lambda streams
        (let loop-1 ((streams streams))
            (if (null? streams)
                stream-null
                (let loop-2 ((s (car streams)))
                    (if (stream-null? s)
                        (loop-1 (cdr streams))
                        (cons
                            (car s)
                            (lambda () (loop-2 ((cdr s)))))))))))

(define-public (list->stream ls)
    (let loop ((ls ls))
        (if (null? ls)
            stream-null
            (cons
                (lambda () (car ls))
                (lambda () (loop (cdr ls)))))))

(define-public (stream->list s)
    (let loop ((r '()) (s s))
        (if (stream-null? s)
            (reverse r)
            (loop
                (cons ((car s)) r)
                ((cdr s))))))
