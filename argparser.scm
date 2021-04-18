(define-module (jlib argparser)
  #:use-module (srfi srfi-1)
  #:use-module (jlib parse)
  #:use-module (jlib strings)
  #:use-module (jlib lists)
  #:use-module (jlib print)
  #:export (parseargs))

;; TODO: allow multiple short flags like -vvv
#|
Useage:
(parseargs
  '((#:num "z" "zebra")
    (#:str "y" "yellow")
    (#:bool "x" "xray" "rontgenradiation"))) ; multiple long forms are allowed, but the first is used in the parsed alist
This will return a alist in this form
'((zebra . #f) (yellow . #f) (xray . 0))

The key is always the long name, the value depends on the type.
For bool: value is the number of times the flag appears
For int: value is #f if the name never appears, or the associated
 value is not a number. Otherwise, its a number representing
 the desired flag eg. -z=2, --zebra=2, -z 2, or --zebra 2 will
 all parse to 2
For str: value is #f if the name never appears. Otherwise, its a
 string representing the desired flag.
 eg. -z=2, --zebra=2, -z 2, or --zebra 2 will all parse to 2
|#

(define (parse/prefix short long)
  (define ss (string-append "-" short))
  (define sls (map (lambda (x) (string-append "--" x)) long))
  (cond
   ((and short long) (apply parse/or_lit ss sls))
   (short (parse/lit ss))
   (long (apply parse/or_lit sls))))

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
  (cons (car full) out))

(define (parse-bool short full)
 (define pref (parse/prefix short full))
 (define count
   (reduce + 0
       (map (lambda (x) (if (eq? (pref x) 'parse-error) 0 1))
           (command-line))))
 (cons (car full) count))

(define (parse-int short full)
  (parse-pair short full string->number (parse/int)))

(define (parse-str short full)
 (parse-pair short full (lambda (x) x) (parse/until (parse/none))))

(define (parse-op op)
  (let ((type (first op))
        (short (second op))
        (full (cddr op))) ;; full is the rest of the arguments
    (case type
      ((#:bool) (parse-bool short full))
      ((#:str) (parse-str short full))
      ((#:num) (parse-int short full)))))

(define (calc-anon takers1 takers2 data)
  (cond ((null? data) '())
        ((any (lambda (x) (starts-with (car data) x)) takers2)
         (if (string-contains (car data) "=")
            (calc-anon takers1 takers2 (cdr data))
            (calc-anon takers1 takers2 (cddr data))))
        ((any (lambda (x) (starts-with (car data) x)) takers1)
         (calc-anon takers1 takers2 (cdr data)))
      (cons (car data) (calc-anon takers1 takers2 (cdr data)))))

(define (anon ops)
  (define (taker-maker filt)
    (flatten
     (map (lambda (x)
            (list
             (string-append "-" (second x))
             (string-append "--" (third x))))
          (filter filt ops))))
  (define takers1 (taker-maker (lambda (x) (eq? #:bool (car x)))))
  (define takers2 (taker-maker (lambda (x) (not (eq? #:bool (car x))))))
  (define data (cdr (command-line)))
  (calc-anon takers1 takers2 data))


(define (parseargs ops)
  (cons
   `(anon . ,(anon ops))
   (map parse-op ops)))
