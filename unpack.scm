(library (unpack)
(export list-unpack)
(import (chezscheme))

(define-syntax list-unpack
  (syntax-rules ()
    ((_ ls vars expr expr* ...)
      (apply (lambda vars expr expr* ...) ls))))

)
