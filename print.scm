(define-module (jlib print)
  #:use-module (ice-9 format)
  #:export (println
            print
            writeln
            dbg
            ansi-code
            ansi-code-list

            colors
            effects

            with-color
            with-effect
            with-effects
            with-foreground
            with-background

            in-box-rounded
            in-box-spaced
            in-box-double
            in-box-thick
            in-box))

(define (dbg v)
  (writeln v)
  v)

(define (writeln x)
  (write x)
  (newline))

(define colors
  '((#:BLK . 0) (#:RED . 1) (#:GRN . 2) (#:YEL . 3)
    (#:BLU . 4) (#:MAG . 5) (#:CYN . 6) (#:WHT . 7)
    (#:RST . 9)
    (#:BRIGHT_BLK . 60) (#:BRIGHT_RED . 61) (#:BRIGHT_GRN . 62) (#:BRIGHT_YEL . 63)
    (#:BRIGHT_BLU . 64) (#:BRIGHT_MAG . 65) (#:BRIGHT_CYN . 66) (#:BRIGHT_WHT . 67)))

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

(define* (in-box-rounded thunk #:key (px 1) (py 0))
  (in-box thunk
          #:px px #:py py
          #:top-left "╭"
          #:top-right "╮"
          #:bottom-left "╰"
          #:bottom-right "╯"))

(define* (in-box-spaced thunk #:key (px 1) (py 0))
  (in-box thunk
          #:px px #:py py
          #:top-left " "
          #:top-right " "
          #:bottom-left " "
          #:bottom-right " "))

(define* (in-box-double thunk #:key (px 1) (py 0))
  (in-box thunk
          #:px px #:py py
          #:left "║" #:right "║"
          #:top #\═ #:bottom #\═
          #:top-left "╔"
          #:top-right "╗"
          #:bottom-left "╚"
          #:bottom-right "╝"))

(define* (in-box-thick thunk #:key (px 1) (py 0))
  (in-box thunk
          #:px px #:py py
          #:left "┃" #:right "┃"
          #:top #\━ #:bottom #\━
          #:top-left "┏"
          #:top-right "┓"
          #:bottom-left "┗"
          #:bottom-right "┛"))


(define (true-length str)
  ;; TODO: one day make this account for all ansi escape sequences
  (define s "")
  (define inesc #f)
  (map (lambda (x)
         (when (and (not inesc) (char=? #\esc x))
            (set! inesc #t))
         (when (not inesc)
          (set! s (string-append s (make-string 1 x))))
         (when (and inesc (char=? #\m x))
            (set! inesc #f)))
       (string->list str))
  (string-length s))

(define* (in-box thunk #:key
                 (px 1) (py 0)
                 (left "│") (right "│")
                 (top #\─) (bottom #\─)
                 (top-left "┌") (top-right "┐")
                 (bottom-left "└") (bottom-right "┘"))
  (define s (with-output-to-string thunk))
  (define s1 (string-trim-right s))
  (define lines (string-split s1 #\newline))
  (define cols 0)
  (define rows 0)

  (map
   (lambda (x)
     (set! cols (max cols (true-length x))))
   lines)
  (set! rows (length lines))

  (set! cols (+ cols (* 2 px)))
  (set! rows (+ rows (* 2 px)))
  (let ((s-top (string-append top-left (make-string cols top) top-right))
        (s-bottom (string-append bottom-left (make-string cols bottom) bottom-right)))
    (println s-top)
    (map (lambda (x)
           (define l (- cols (true-length x) (* 2 px)))
           (print left)
           (print (make-string px #\space))
           (print x)
           (print (make-string l #\space))
           (print (make-string px #\space))
           (println right))
         lines)
    (println s-bottom)))




; Usage example
; (with-color #:YEL #:BLU (display "Hello, World!"))
; (println)
; (with-foreground #:BLU (display "Hello, World!"))
; (println)
; (with-background #:BLU (display "Hello, World!"))
; (println)


;(in-box-rounded
; (lambda ()
;   (println "Hello, World")
;   (with-color #:BLK #:RED (print "Hm")) (println)
;   (println "Hello, World")))
