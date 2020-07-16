(define-module (jlib print)
  #:use-module (ice-9 format)
  #:export (println
            print
            ansi-code
            ansi-code-list

            colors
            effects

            with-color
            with-effect
            with-effects
            with-foreground
            with-background))

(define colors
  '((#:BLK . 0) (#:RED . 1) (#:GRN . 2) (#:YEL . 3)
    (#:BLU . 4) (#:MAG . 5) (#:CYN . 6) (#:WHT . 7)
    (#:RST . 9)))

(define effects
  '((#:RST . 0)
    (#:BOLD . 1)
    (#:FAINT . 2)
    (#:ITALIC . 3)
    (#:UNDERLINE . 4)
    (#:BLINK . 5)
    (#:FBLINK . 6)
    (#:REVERSE . 7)
    (#:CONCEAL . 8)
    (#:STRIKE . 9)))

(define (print . items)
  (format #t "~{~A~^ ~}" items))

(define (println . items)
  (format #t "~{~A~^ ~}~%" items))

(define (ansi-code-list l)
  (apply ansi-code l))

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

(define-macro (with-effect effect fn)
  `(begin
     (ansi-code (assoc-ref effects ,effect) #\m)
     ,fn
     (ansi-code (assoc-ref effects #:RST) #\m)))

(define-macro (with-effects teffects fn)
  `(begin
     (ansi-code-list (append
                      (map (λ (x) (assoc-ref effects x)) ,teffects)
                      (list #\m)))
     ,fn
     (ansi-code (assoc-ref effects #:RST) #\m)))

; Usage example
; (with-color #:YEL #:BLU (display "Hello, World!"))
; (println)
; (with-foreground #:BLU (display "Hello, World!"))
; (println)
; (with-background #:BLU (display "Hello, World!"))
; (println)

