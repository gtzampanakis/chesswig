(library (util)
(export
  d
  sum
  prod
  range
  same
  range-cycle
  minabs-index
  proc-prepend-arg
  index
  take-n-or-fewer
  string-split
  first
  last
  list-set!)
(import
  (chezscheme))

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

(define (sum ls)
  (fold-left + 0 ls))

(define (prod ls)
  (fold-left * 1 ls))

(define (range-inner s n)
; List of integers >= s and < n.
  (let loop ((i n) (r '()))
    (if (<= i s)
      r
      (loop (- i 1) (cons (- i 1) r)))))

(define range
  (case-lambda
    ((n) (range-inner 0 n))
    ((s n) (range-inner s n))))

(define (same n l)
  (if (= l 0)
    '()
    (cons n (same n (- l 1)))))

(define (range-cycle s n)
; Improper list of integers >= s and < n.
  (let ((ls (range s n)))
    (if (null? ls)
      ls
      (let loop ((pair ls))
        (if (null? (cdr pair))
          (begin
            (set-cdr! pair ls)
            ls)
          (loop (cdr pair)))))))

(define (minabs-index ls)
  (let loop ((ls ls) (c (+inf.0)) (i 0) (r 0))
    (if (null? ls)
      r
      (let* (
          (a (abs (car ls)))
          (o (< a c)))
        (loop
          (cdr ls)
          (if o a c)
          (+ i 1)
          (if o i r))))))

(define (proc-prepend-arg f)
  (lambda args
    (apply f (cdr args))))

(define (index ls val)
  (let loop ((ls ls) (r 0))
    (if (null? ls)
      #f
      (if (equal? (car ls) val)
        r
        (loop (cdr ls) (+ r 1))))))

(define (take-n-or-fewer ls n)
  (cond
    ((null? ls) '())
    ((= n 0) '())
    (else
      (cons
        (car ls)
        (take-n-or-fewer (cdr ls) (- n 1))))))

(define (take ls n)
  (if (= n 0)
    '()
    (cons (car ls) (take (cdr ls) (- n 1)))))

(define (drop ls n)
  (if (= n 0)
    ls
    (drop (cdr ls) (- n 1))))

(define (string-split s c)
  (let loop ((strings '()) (ls '()) (chars (string->list s)) (return #f))
    (if (null? chars)
      (if return
        (reverse strings)
        (loop strings ls (list c) #t))
      (let ((char (car chars)))
        (let ((char-matches? (char=? char c)))
          (loop
            (if char-matches?
              (cons (apply string (reverse ls)) strings)
              strings)
            (if char-matches?
              '()
              (cons char ls))
            (cdr chars)
            return))))))

(define (first ls) (car ls))

(define (last ls)
  (let ((cdr-ls (cdr ls)))
    (if (null? cdr-ls)
      (car ls)
      (last cdr-ls))))

(define (list-set! ls i v)
  (let loop ((ls ls) (i i))
    (if (= i 0)
      (set-car! ls v)
      (loop (cdr ls) (- i 1)))))

)
