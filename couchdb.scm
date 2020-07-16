(define-module (jlib couchdb)
  #:use-module (jlib print)
  #:use-module (jlib uuid)
  #:use-module (jlib base64)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:export (couch-root
            make-couchdb

            couch-all-dbs
            couch-new-db
            couch-delete-db

            couch-all-documents
            couch-new-document
            couch-get-document
            couch-get-document-by-field
            couch-delete-document))

;;; CouchDB does not support Authorization: basic, it
;;; requires Authorization: Basic, despite the rfc saying
;;; it should be case insensitive. This gets around that
;;; by making guile send it as a capital
(define (write-credentials val port)
  (match val
    (('basic . cred)
     ((@@ (web http) put-string) port "Basic ")
     ((@@ (web http) put-string) port cred))
    ((scheme . params)
     ((@@ (web http) put-symbol) port scheme)
     ((@@ (web http) put-char) port #\space)
     ((@@ (web http) write-key-value-list) params port))))

(define (declare-credentials-header! name)
  (declare-header! name
    (@@ (web http) parse-credentials) (@@ (web http) validate-credentials) write-credentials))

(declare-credentials-header! "Authorization")

(module-set! (resolve-module '(web http)) 'write-credentials write-credentials)
(module-set! (resolve-module '(web http)) 'declare-credentials-header! declare-credentials-header!)
;;; end weird hacky guile stuff



(define-record-type <couchdb>
  (make-couchdb location port user password)
  couchdb?
  (location couchdb-location set-couchdb-location!)
  (port     couchdb-port     set-couchdb-port!)
  (user     couchdb-user     set-couchdb-user!)
  (password couchdb-password set-couchdb-password!))


(make-record-type "couchdb" '(location port user password))

(define (loc dbs endpoint)
  (define location (couchdb-location dbs))
  (define port (couchdb-port dbs))
  (define user (couchdb-user dbs))
  (define password (couchdb-password dbs))
  (format #f "http://~a:~a/~a" location port endpoint))

(define (auth-header dbs)
  (define user (couchdb-user dbs))
  (define password (couchdb-password dbs))
  (define b64 (base64-encode (string->utf8 (format #f "~a:~a" user password))))
  `(basic . ,b64))

(define* (error code #:optional body)
  `((error . ,code)
    (body . ,body)))

(define (error? result)
  (if (pair? result)
      (assoc 'error result)
      #f))

(define* (couch-request dbs verb endpoint rbody)
  (define headers `((content-type . (application/json))
                    (authorization . ,(auth-header dbs))))
  (define uri (loc dbs endpoint))
  (define-values (res body)
    (if rbody
        (http-request uri #:method verb #:headers headers #:body rbody)
        (http-request uri #:method verb #:headers headers)))

  (if (and (< (response-code res) 300) (>= (response-code res) 200))
    (json-string->scm (utf8->string body))
    (error (response-code res) (json-string->scm (utf8->string body)))))

(define (get dbs endpoint) (couch-request dbs 'GET endpoint #f))
(define* (put dbs endpoint #:optional (body #f)) (couch-request dbs 'PUT endpoint body))
(define* (post dbs endpoint #:optional (body #f)) (couch-request dbs 'POST endpoint body))
(define (delete dbs endpoint) (couch-request dbs 'DELETE endpoint #f))


(define (couch-root dbs) (get dbs ""))
(define (couch-all-dbs dbs) (get dbs "_all_dbs"))
(define (couch-new-db dbs db) (put dbs db))
(define (couch-delete-db dbs db) (delete dbs db))

(define (couch-all-documents dbs db) (get dbs (format #f "~a/_all_docs" db)))
(define (couch-new-document dbs db item)
  (define id (uuidgen))
  (define res (put dbs (format #f "~a/~a" db id) (scm->json-string item)))
  (if (error? res) res id))

(define (couch-get-document dbs db id) (get dbs (format #f "~a/~a" db id)))
(define* (couch-get-document-by-field dbs db field value #:optional (fields #f))
  (define pbody `(("selector" (,field . ,value)) ("limit" . 1)))
  (define body (if fields (acons "fields" fields pbody) pbody))
  (define res (post dbs (format #f "~a/_find" db) (scm->json-string body)))
  (define val (assoc-ref res "docs"))
  (if (error? res) res
      (if (and (vector? val) (= 1 (vector-length val)))
          (vector-ref val 0)
          (error 404))))
(define (couch-delete-document dbs db id rev)
  (delete dbs (format #f "~a/~a?rev=~a" db id rev)))


