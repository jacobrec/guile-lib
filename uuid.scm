(define-module (jlib uuid)
  #:use-module (jlib shell)
  #:export (uuidgen))

(define (uuidgen)
  (shell "uuidgen"))
