(define-module (jlib random)
  #:export (random:list
            random:vector))

(define (random:list l)
  (list-ref l (random (length l))))
(define (random:vector l)
  (vector-ref l (random (vector-length l))))
