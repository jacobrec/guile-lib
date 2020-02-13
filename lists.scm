(define-module (jlib lists)
  #:export (cartisian-product))


(define (cartisian-product l1 . rest)
  (define (cp acc l1 . rest)
    (if (null? rest)
     (apply append (map (λ (x) (map (λ (y) (cons x y)) acc)) l1))
     (apply cp (apply append
                      (map (λ (x)
                             (map (λ (y)
                                    (cons x y))
                                  acc))
                           l1))
            rest)))
  (apply cp (map (λ (x) (list x)) l1) rest))