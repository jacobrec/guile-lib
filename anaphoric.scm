(define-module (jlib lists)
  #:export (aif))

(define-macro (aif test-form then-form else-form)
  `(let ((it ,test-form))
      (if it ,then-form else-form)))

