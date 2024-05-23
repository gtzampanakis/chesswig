(library (unpack)
(export list-unpack for-each-in)
(import (chezscheme))

(define-syntax list-unpack
  (syntax-rules ()
    ((_ ls vars expr expr* ...)
      (apply (lambda vars expr expr* ...) ls))))

(define-syntax for-each-in
  (syntax-rules ()
    ((_ (name ...) ls expr expr* ...)
      (for-each
        (lambda (a) (list-unpack a (name ...) expr expr* ...)) ls))))

)
