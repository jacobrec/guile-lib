(define-module (jlib shell)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (jlib print)
  #:export (shell
            shell-line
            subprocess
            edit))

;; Runs a shell command and returns stdout as a string
(define (shell cmd)
  (let* ((port (open-input-pipe cmd))
         (str  (get-string-all port)))
    (close-pipe port)
    (string-trim-right str)))

;; Runs a shell command with stdin from a string
;; Doesn't work
(define* (shell-line cmd #:key (input ""))
  (with-input-from-string input
    (lambda ()
      (shell cmd))))

;; Needed for piped-process
(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_popen"))

;; Runs a subprocess ex) (subprocess "vim tmpfile")
(define (subprocess cmd-str)
  (define cmd-args (string-split cmd-str #\space))
  (define cmd (car cmd-args))
  (define args (cdr cmd-args))
  (define pid (piped-process cmd args #f #f))
  (waitpid pid))

;; Opens a data from a string in an editor, then returns the edited string data
(define* (edit #:optional (data #f))
  (define filename (port-filename (mkstemp! (string-copy "/tmp/jlife-XXXXXXXX"))))
  (when data
    (with-output-to-file filename
      (lambda ()
        (display data))))

  (when (< (string-length (getenv "VISUAL")) 1)
    (println "Please ensure $VISUAL is set properly"))
  (subprocess (string-append (getenv "VISUAL") " " filename))
  (let ((data (get-string-all (open-input-file filename))))
    (delete-file filename)
    data))

