(define-module (jlib parse)
  #:use-module (srfi srfi-11) ; for let-values
  #:use-module (jlib strings) ; for starts-with
  #:export (parse/lit
            parse/and
            parse/or
            parse/apply

            parse/*
            parse/+
            parse/?

            parse/and_lit
            parse/or_lit

            parse/digit
            parse/int
            parse/float
            parse/quoted-string

            parse/between

            ignore-whitespace))

(define ignore-whitespace (make-parameter #t))

(define (parse/lit value)
 (define l (string-length value))
 (λ (str)
   (define tstr (if (ignore-whitespace) (string-trim str) str))
   (if (and
        (>= (string-length tstr) l)
        (string= value (substring tstr 0 l)))
    (values value (substring tstr l))
    (values 'parse-error str))))

(define (parse/or2 v1 v2)
  (λ (str)
    (define-values (parsed res) (v1 str))
    (if (eq? parsed 'parse-error)
        (v2 str)
        (values parsed res))))

(define (parse/and2 v1 v2)
  (λ (str)
    (define-values (parsed res) (v1 str))
    (if (eq? parsed 'parse-error)
        (values 'parse-error str)
        (let-values (((parsed2 res) (v2 res)))
          (if (eq? parsed2 'parse-error)
            (values 'parse-error str)
            (values (cons parsed parsed2) res))))))

(define (parse/nil)
  (λ (str)
    (values '() str)))

(define (parse/or v1 . rest)
  (if (null? rest) v1
    (parse/or2 v1 (apply parse/or rest))))

(define (parse/and v1 . rest)
  (if (null? rest)
    (parse/and2 v1 (parse/nil))
    (parse/and2 v1 (apply parse/and rest))))

(define (parse/or_lit . rest)
  (apply parse/or (map (λ (v) (parse/lit v))
                       rest)))

(define (parse/and_lit . rest)
  (apply parse/and (map (λ (v) (parse/lit v))
                        rest)))

(define (parse/digit)
  (parse/or_lit "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define (parse/* v1)
  (λ (str)
    (define (loop res str)
      (let-values (((r2 str2) (v1 str)))
        (if (eq? r2 'parse-error)
          (values res str)
          (loop (cons r2 res) str2))))
    (let-values (((finalres finalstr) (loop '() str)))
      (values (reverse finalres) finalstr))))

(define (parse/apply v1 fn)
  (λ (str)
    (let-values (((res str) (v1 str)))
      (values (if (eq? res 'parse-error)
                  'parse-error
                  (fn res))
              str))))

(define (parse/+ v1)
  (parse/apply
    (parse/and v1 (parse/* v1))
    (λ (parsed)
      (cons (car parsed) (cadr parsed)))))

(define (parse/? v1)
  (parse/or v1 (parse/nil)))

(define (parse/int)
  (λ (str)
    (define tstr (if (ignore-whitespace) (string-trim str) str))
    (parameterize ((ignore-whitespace #f))
      ((parse/apply
        (parse/+ (parse/digit))
        (λ (parsed)
          (string->number (apply string-append parsed))))
       tstr))))

(define (parse/float)
  (λ (str)
   (define tstr (if (ignore-whitespace) (string-trim str) str))
   ;(format #t "parsing float from: ~A~%" tstr)
   (parameterize ((ignore-whitespace #f))
     (let*-values (((res tstr) ((parse/int) tstr))
                   ((res2 tstr) ((parse/and2 (parse/lit ".") (parse/int)) tstr)))
      (define fstr (format #f "~A.~A"
                          (if (eq? res 'parse-error) "0" res)
                          (if (eq? res2 'parse-error) "0" (cdr res2))))
      (values (if (eq? res res2)
                  'parse-error
                  (string->number fstr))
              tstr)))))

(define* (parse/quoted-string #:optional (quote-lit "\"") (escape-lit "\\"))
  (define l (string-length quote-lit))
  (λ (str)
    (define (loop str acc)
      (define c (string-take str 1))
      (cond ((string= c escape-lit) (loop (string-drop str 2) (cons (string-drop (string-take str 2) 1) acc)))
            ((string= c quote-lit) (string-concatenate-reverse acc))
            (else (loop (string-drop str 1) (cons c acc)))))

   (if (starts-with str quote-lit)
       (loop (substring str l) '())
       (values 'parse-error str))))

(define (parse/between left middle right)
  (parse/apply
    (parse/and left middle right)
    (λ (parsed) (cadr parsed))))

; (format #t "~a" ((parse/quoted-string) "\"hello\\\"\""))
; ((parse/int) "12")
