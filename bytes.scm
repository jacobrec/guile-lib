(define-module (jlib bytes)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (bytevector-append
            bytevector-subvector))
(define (bytevector-append . args)
  (define len (fold (lambda (x acc) (+ acc (bytevector-length x))) 0 args))
  (define idx 0)
  (define bv (make-bytevector len))
  (map (lambda (x)
         (bytevector-copy! x 0 bv idx (bytevector-length x))
         (set! idx (+ idx (bytevector-length x)))
         x) args)
  bv)

(define (bytevector-subvector bv idx len)
  (define bvn (make-bytevector len))
  (bytevector-copy! bv idx bvn 0 len)
  bvn)
