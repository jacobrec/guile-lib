(define-module (jlib print)
  #:use-module (ice-9 format)
  #:export (println
            ansi-code
            with-color
            with-foreground
            with-background))

(define colors
  '((#:BLK . 0) (#:RED . 1) (#:GRN . 2) (#:YEL . 3)
    (#:BLU . 4) (#:MAG . 5) (#:CYN . 6) (#:WHT . 7)
    (#:RST . 9)))

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


(define-macro (with-color fg bg fn)
  `(begin
     (ansi-code (+ 30 (assoc-ref colors ,fg))
                (+ 40 (assoc-ref colors ,bg))
                #\m)
     ,fn
     (ansi-code (+ 30 (assoc-ref colors #:RST))
                (+ 40 (assoc-ref colors #:RST))
                #\m)))

(define-macro (with-foreground fg fn)
  `(begin
     (ansi-code (+ 30 (assoc-ref colors ,fg)) #\m)
     ,fn
     (ansi-code (+ 30 (assoc-ref colors #:RST)) #\m)))

(define-macro (with-background bg fn)
  `(begin
     (ansi-code (+ 40 (assoc-ref colors ,bg)) #\m)
     ,fn
     (ansi-code (+ 40 (assoc-ref colors #:RST)) #\m)))

; Usage example
; (with-color #:YEL #:BLU (display "Hello, World!"))
; (println)
; (with-foreground #:BLU (display "Hello, World!"))
; (println)
; (with-background #:BLU (display "Hello, World!"))
; (println)
