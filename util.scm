(define-module (util))

(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-43))

(define-public d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

(define-public (time)
  (let ((p (gettimeofday))) (+ (car p) (/ (cdr p) 1000000.))))

(define-public (sum ls)
  (fold + 0 ls))

(define-public (prod ls)
  (fold * 1 ls))

(define-public range
    (lambda args
        (define (range-inner s n)
        ; List of integers >= s and < n.
          (let loop ((i n) (r '()))
            (if (<= i s)
              r
              (loop (1- i) (cons (1- i) r)))))
        (match args
            ((n) (range-inner 0 n))
            ((s n) (range-inner s n)))))

(define-public (same n l)
  (if (= l 0)
    '()
    (cons n (same n (1- l)))))

(define-public (range-cycle s n)
; Improper list of integers >= s and < n.
  (unless (integer? s)
    (error 'non-integer-s))
  (unless (integer? n)
    (error 'non-integer-n))
  (let ((ls (range s n)))
    (if (null? ls)
      ls
      (let loop ((pair ls))
        (if (null? (cdr pair))
          (begin
            (set-cdr! pair ls)
            ls)
          (loop (cdr pair)))))))

(define-public (flatten ls)
  (fold append '() ls))

(define-public (minabs-index ls)
  (let loop ((ls ls) (c (inf)) (i 0) (r 0))
    (if (null? ls)
      r
      (let* (
          (a (abs (car ls)))
          (o (< a c)))
        (loop
          (cdr ls)
          (if o a c)
          (1+ i)
          (if o i r))))))

(define-public (proc-prepend-arg f)
  (lambda args
    (apply f (cdr args))))

(define-public (index ls val)
  (let loop ((ls ls) (r 0))
    (if (null? ls)
      #f
      (if (equal? (car ls) val)
        r
        (loop (cdr ls) (1+ r))))))

(define-public (take-n-or-fewer ls n)
    (cond
        ((null? ls) '())
        ((= n 0) '())
        (else
            (cons
                (car ls)
                (take-n-or-fewer (cdr ls) (1- n))))))
