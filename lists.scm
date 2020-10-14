(define-module (jlib lists)
  #:export (cartisian-product
            flatten))


(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

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
