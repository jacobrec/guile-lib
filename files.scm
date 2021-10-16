(define-module (jlib files)
  #:use-module (ice-9 format)
  #:export (mkdir-if))

(define (mkdir-if path)
  (if (file-exists? path)
    (values)
    (mkdir path)))
