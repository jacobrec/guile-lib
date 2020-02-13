(define-module (jlib print)
  #:export (println))

(define (println . items)
  (format #t "~{~A~^ ~}~%" items))

(define (ansi-code val . rest)
  (define (inner val . rest)
   (cond ((null? val) 0)
         ((null? rest) (format #t "~A" val))
         ((null? (cdr rest)) (format #t ";~A~A" val (car rest)))
         (else (format #t ";~A" val) (apply inner rest))))
  (format #t "~A[~A" #\esc val)
  (unless (null? rest)
    (apply inner rest)))


(println 1 2 4)
