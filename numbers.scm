(define-module (jlib numbers)
  #:export (between?))

(define (between? a b c)
  (and (<= a b) (<= b c)))
