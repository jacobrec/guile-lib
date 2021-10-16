(define-module (jlib lists)
  #:use-module (srfi srfi-1)
  #:export (cartisian-product
            flatten
            assoc-get
            alist-merge))


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


(define* (assoc-get key alist #:optional default)
  (define v (assoc key alist))
  (if v (cdr v) default))

(define (alist-merge alist newdata)
  (fold
   (lambda (x acc)
     (assoc-set! alist (car x) (cdr x)))
   alist newdata))
