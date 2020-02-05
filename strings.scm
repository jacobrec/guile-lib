(define-module (jlib strings)
  #:export (starts-with))

(define (starts-with str check)
  (string= check (substring str 0 (string-length check))))
