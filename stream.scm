(define-module (stream))

; A stream is a thunk which returns either:
; * A pair whose
;       * car is a thunk that returns a value and whose
;       * cdr is a thunk that returns a stream or
; * null

(use-modules (util))

(define-public stream-null (lambda () '()))

(define-public (stream-null? obj)
    (null? (obj)))

(define-public (list->stream ls)
    (let loop ((ls ls))
        (if (null? ls)
            stream-null
            (lambda ()
                (cons
                    (lambda () (car ls))
                    (lambda () (loop (cdr ls))))))))

(define-public (stream->list s)
    (let loop ((r '()) (s s))
        (let ((sp (s)))
            (if (null? sp)
                (reverse r)
                (loop
                    (cons ((car sp)) r)
                    ((cdr sp)))))))

(define-public (stream-for-each proc s)
    (let loop ((s s))
        (let ((sp (s)))
            (unless (null? sp)
                (proc ((car sp)))
                (loop ((cdr sp)))))))

(define-public (stream-map proc s)
    (let loop ((s s))
        (let ((sp (s)))
            (if (null? sp)
                stream-null
                (lambda ()
                    (cons
                        (lambda () (proc ((car sp))))
                        (lambda () (loop ((cdr sp))))))))))

(define-public stream-append 
    (lambda streams
        (let loop-1 ((streams streams))
            (if (null? streams)
                stream-null
                (let loop-2 ((s (car streams)))
                    (let ((sp (s)))
                        (if (null? sp)
                            (loop-1 (cdr streams))
                            (lambda ()
                                (cons
                                    (car sp)
                                    (lambda () (loop-2 ((cdr sp)))))))))))))

