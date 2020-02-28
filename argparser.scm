(define-module (jlib argparser)
  #:use-module (srfi srfi-1)
  #:use-module (jlib parse)
  #:use-module (jlib print)
  #:export (parseargs))

(define (parse/prefix short long)
  (define ss (string-append "-" short))
  (define sl (string-append "--" long))
  (cond
   ((and short long) (parse/or_lit ss sl))
   (short (parse/lit ss))
   (long (parse/lit sl))))

(define (parse/arg-with-val pref res-parser)
  (parse/and
    pref
    (parse/lit "=")
    res-parser))

(define (locate-arg parser)
  (define (inner cl)
    (cond
     ((null? cl) #f)
     ((eq? (parser (car cl)) 'parse-error) (inner (cdr cl)))
     (else cl)))
  (inner (command-line)))

(define (parse-pair short full conversion parser)
  (define pref (parse/prefix short full))
  (define cl (locate-arg pref))
  (define-values (parsed res) ((parse/arg-with-val pref parser)
                               (or (and cl (car cl)) "")))
  (define out (if cl
                  (if (eq? parsed 'parse-error)
                      (conversion (second cl))
                      (third parsed))
                  #f))
  (cons full out))

(define (parse-bool short full)
 (define pref (parse/prefix short full))
 (define count
   (reduce + 0
       (map (lambda (x) (if (eq? (pref x) 'parse-error) 0 1))
           (command-line))))
 (cons full count))

(define (parse-int short full)
  (parse-pair short full string->number (parse/int)))

(define (parse-str short full)
 (parse-pair short full (lambda (x) x) (parse/until (parse/none))))

(define (parse-op op)
  (let ((type (first op))
        (short (second op))
        (full (third op)))
    (case type
      ((#:bool) (parse-bool short full))
      ((#:str) (parse-str short full))
      ((#:num) (parse-int short full)))))

(define (parseargs ops)
  (map parse-op ops))
