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
  take
  drop
  take-n-or-fewer
  string-split
  first
  last
  list-set!
  seconds-since-epoch
  argmax
  make-queue
  worker-proc
  parallel-map
  rec<
  string-join
  )
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

(define (string-join ls)
  (if
    (null? ls)
    ""
    (if (null? (cdr ls))
      (car ls)
      (string-append
        (car ls)
        " "
        (string-join (cdr ls))))))

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

(define (seconds-since-epoch)
  (let ((t (current-time)))
    (let ((s (time-second t)) (n (time-nanosecond t)))
      (+ s (/ n 1000000000)))))

(define (argmax ls less)
  (let ((h (car ls)) (t (cdr ls)))
    (if (null? t)
      h
      (let ((argmax-t (argmax t less)))
        (cond
          ((less h argmax-t)
            argmax-t)
          ((less argmax-t h)
            h)
          (else
            h))))))

(define-syntax queue-push!
  (syntax-rules ()
    ((_ obj ls)
      (set! ls (cons obj ls)))))

(define-syntax queue-pop!
  (syntax-rules ()
    ((_ ls)
      (if (null? ls)
        #f
        (let ((r (car ls)))
          (set! ls (cdr ls))
          r)))))

(define (make-queue)
; Taken from https://irreal.org/blog/?p=40
    (let (
        (front '()) (back '())
        (mutex (make-mutex)) (condition (make-condition)))
      (lambda (cmd . data)
        (define exchange
          (lambda ()
            (set! front (reverse back))
            (set! back '())))
        (case cmd
          ((push)
            (with-mutex mutex
              (queue-push! (car data) back)
              (condition-signal condition)))
          ((pop) (with-mutex mutex
                    (or
                      (queue-pop! front)
                      (begin
                        (exchange)
                        (queue-pop! front))
                      (begin
                        (condition-wait condition mutex)
                        (or
                          (queue-pop! front)
                          (begin
                            (exchange)
                            (queue-pop! front)))))))
          ((peek) (unless (pair? front)
                    (exchange))
                    (car front))
          ((show) (format #t "~s\n" (append front (reverse back))))
          ((fb) (format #t "front: ~s, back: ~s\n" front back))
          (else (error "Illegal cmd to queue object" cmd))))))

(define (worker-proc q-input q-output)
  (let loop ()
    (let ((obj (q-input 'pop)))
      (apply
        (lambda (id proc args)
          (let ((result (apply proc args)))
            (q-output 'push (list id result))))
        obj)
      (loop))))

(define (parallel-map f ls q-worker-input q-worker-output)
  (let ((result-ls (make-list (length ls))))
    (let loop ((i 0) (ls ls))
      (unless (null? ls)
        (q-worker-input 'push (list i f (list (car ls))))
        (loop (+ i 1) (cdr ls))))
    (let loop ((ndone 0))
      (when (< ndone (length ls))
        (let ((output-obj (q-worker-output 'pop)))
          (apply
            (lambda (id result)
              (list-set! result-ls id result))
            output-obj))
        (loop (+ ndone 1))))
    result-ls))

(define (rec< a b)
  (define (rec= a b)
    (and (not (rec< b a)) (not (rec< a b))))
  (cond
    ((and (number? a) (number? b)) (< a b))
    ((and (number? a) (list? b)) -1)
    ((and (list? a) (number? b)) 1)
    ((and (list? a) (list? b))
      (let (
          (la (length a))
          (lb (length b)))
        (if (= la lb)
          (let loop ((a a) (b b))
            (if (null? a) 0
              (let ((ai (car a)) (bi (car b)))
                (if (rec= ai bi)
                  (loop (cdr a) (cdr b))
                  (rec< ai bi)))))
          (rec< la lb))))
    (else (raise "rec<: Unexpected types"))))

)
