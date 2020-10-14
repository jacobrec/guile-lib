(define-module (jlib strings)
  #:export (starts-with))

(define (starts-with str check)
  (define sl_str (string-length str))
  (define sl_check (string-length check))
  (and (>= sl_str sl_check)
    (string= check (substring str 0 sl_check))))
