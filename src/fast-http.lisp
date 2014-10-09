(in-package :cl-user)
(defpackage fast-http
  (:use :cl
        :fast-http.parser
        :fast-http.byte-vector
        :fast-http.error)
  (:import-from :babel
                :octets-to-string)
  (:export :make-http-parser
           :http
           :http-request
           :make-http
           :make-http-request
           :http-version
           :http-headers
           :http-body
           :http-method
           :http-resource

           ;; Low-level parser API
           :http-parse
           :parser
           :make-parser
           :make-parser-callbacks
           :parser-method
           :parser-status-code
           :parser-http-major
           :parser-http-minor

           ;; Error
           :fast-http-error

           :callback-error
           :cb-message-begin
           :cb-url
           :cb-header-field
           :cb-header-value
           :cb-headers-complete
           :cb-body
           :cb-message-complete
           :cb-status

           :parsing-error
           :invalid-eof-state
           :header-overflow
           :closed-connection
           :invalid-version
           :invalid-status
           :invalid-method
           :invalid-url
           :invalid-host
           :invalid-port
           :invalid-path
           :invalid-query-string
           :invalid-fragment
           :lf-expected
           :invalid-header-token
           :invalid-content-length
           :invalid-chunk-size
           :invalid-constant
           :invalid-internal-state
           :strict-error
           :paused-error
           :unknown-error))
(in-package :fast-http)

(defstruct http
  version
  headers
  store-body
  body)

(defstruct (http-request (:include http)
                         (:conc-name #.(string :HTTP-)))
  method
  resource)

(defstruct (http-response (:include http)
                          (:conc-name #.(string :HTTP-)))
  status
  status-text)

(defun make-http-parser (http &key header-callback body-callback finish-callback store-body)
  (let* ((headers '())
         (completep nil)
         (parser (make-parser))
         (callbacks (make-parser-callbacks
                     :status (and (http-response-p http)
                                  (lambda (parser data start end)
                                    (setf (http-status http)
                                          (parser-status-code parser))
                                    (setf (http-status-text http)
                                          (babel:octets-to-string data :start start :end end))))
                     :header-field (and header-callback
                                        (lambda (parser data start end)
                                          (declare (ignore parser))
                                          (push (intern (ascii-octets-to-upper-string data :start start :end end)
                                                        :keyword)
                                                headers)
                                          (push '() headers)))
                     :header-value (and header-callback
                                        (lambda (parser data start end)
                                          (declare (ignore parser))
                                          (let ((val (babel:octets-to-string data :start start :end end)))
                                            (if (car headers)
                                                (push val (car headers))
                                                (rplaca headers (list val))))))
                     :headers-complete (and header-callback
                                            (lambda (parser)
                                              (setf (http-version http)
                                                    (+ (parser-http-major parser)
                                                       (/ (parser-http-minor parser) 10)))
                                              (setf (http-method http) (parser-method parser))
                                              (let ((complete-headers '()))
                                                (loop for (val key) on headers by #'cddr
                                                      if (cdr val)
                                                        do (progn
                                                             (push (apply #'concatenate 'string val) complete-headers)
                                                             (push key complete-headers))
                                                      else
                                                        do (progn
                                                             (push (car val) complete-headers)
                                                             (push key complete-headers)))
                                                (setf (http-headers http) complete-headers)
                                                (funcall header-callback complete-headers))))
                     :url (lambda (parser data start end)
                            (declare (ignore parser))
                            (setf (http-resource http)
                                  (babel:octets-to-string data :start start :end end)))
                     :body (and body-callback
                                (lambda (parser data start end)
                                  (declare (ignore parser))
                                  (let ((body-bytes (subseq data start end)))
                                    (when store-body
                                      (setf (http-body http) store-body))
                                    (funcall body-callback body-bytes))))
                     :message-complete (lambda (parser)
                                         (declare (ignore parser))
                                         (setq completep t)))))
    (setf (http-store-body http) store-body)
    (return-from make-http-parser
      (lambda (data)
        (cond
          ((eql data :eof)
           (when finish-callback
             (funcall finish-callback)))
          (T (http-parse parser callbacks data)
             (when (and completep finish-callback)
               (funcall finish-callback))))
        (values http t completep)))))
