(define-module (jlib shell)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (shell
            shell-line))

(define (shell cmd)
  (let* ((port (open-input-pipe cmd))
         (str  (get-string-all port)))
    (close-pipe port)
    (string-trim-right str)))

(define* (shell-line cmd #:key (input ""))
  (let ((port (open-input-output-pipe cmd)))
    (display input port)
    (newline port)
    (let ((s (string-trim-right (get-line port))))
      (close-pipe port)
      s)))


