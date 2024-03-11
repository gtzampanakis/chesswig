(define-module (stream))

; A stream is a thunk which returns either:
; * A pair whose
;     * car is a thunk that returns a value and whose
;     * cdr is a thunk that returns a stream or
; * stream-null

(export
  stream-null
  stream-null?
  stream-cons
  stream-lambda
  define-stream
  stream-for-each
  stream-append
  stream-filter
  stream-map
  stream-fold
  stream->list
  list->stream
  stream)

(use-modules (util))

(define stream-null-obj '~~~stream-null~~~)

(define stream-null (lambda () stream-null-obj))

(define (stream-null? obj)
  (and
    (procedure? (obj))
    (eq? (obj) stream-null-obj)))

(define-syntax stream-cons
  (syntax-rules ()
    ((_ object-expr stream-expr)
      (lambda ()
        (cons
          (lambda () object-expr)
          (lambda () stream-expr))))))

(define-syntax stream-lambda
  (syntax-rules ()
    ((_ formals body body* ...)
      (lambda formals
        (lambda ()
          (let ((s (let () body body* ...)))
            (s)))))))

(define-syntax define-stream
  (syntax-rules ()
    ((_ (name . args) body body* ...)
      (define name
        (stream-lambda args
          (let () body body* ...))))))

(define (stream-for-each proc s)
  (let loop ((s s))
    (let ((sp (s)))
      (unless (eq? sp stream-null-obj)
        (proc ((car sp)))
        (loop ((cdr sp)))))))

(define-stream (stream-append . streams)
  (let loop-1 ((streams streams))
    (if (null? streams)
      stream-null
      (let loop-2 ((s (car streams)))
        (let ((sp (s)))
          (if (eq? sp stream-null-obj)
            (loop-1 (cdr streams))
            (stream-cons
              ((car sp))
              (loop-2 ((cdr sp))))))))))

(define-stream (stream-filter p s)
  (let loop ((s s))
    (let ((sp (s)))
      (if (eq? sp stream-null-obj)
        stream-null
        (let ((v ((car sp))))
          (if (p v)
            (stream-cons
              v
              (loop ((cdr sp))))
            (loop ((cdr sp)))))))))

(define-stream (stream-map p s)
  (let loop ((s s))
    (let ((sp (s)))
      (if (eq? sp stream-null-obj)
        stream-null
        (stream-cons
          (p ((car sp)))
          (loop ((cdr sp))))))))

(define (stream-fold proc init s)
  (let loop ((init init) (s s))
    (let ((sp (s)))
      (if (eq? sp stream-null-obj)
        init
        (let ((v (proc init ((car sp)))))
          (loop v ((cdr sp))))))))

(define (stream->list s)
  (let loop ((s s) (r '()))
    (let ((sp (s)))
      (if (eq? sp stream-null-obj)
        r
        (loop ((cdr sp)) (cons ((car sp)) r))))))

(define-stream (list->stream ls)
  (let loop ((ls ls))
    (if (null? ls)
      stream-null
      (stream-cons
        (car ls)
        (loop (cdr ls))))))

(define-stream (stream . args)
  (let loop ((args args))
    (if (null? args)
      stream-null
      (stream-cons
        (car args)
        (loop (cdr args))))))
