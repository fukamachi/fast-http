(in-package :cl-user)
(defpackage fast-http
  (:use :cl
        :fast-http.parser
        :fast-http.byte-vector
        :fast-http.subseqs
        :fast-http.error)
  (:import-from :fast-http.util
                :make-collector
                :number-string-p)
  (:import-from :babel
                :octets-to-string)
  (:import-from :cl-utilities
                :with-collectors)
  (:export :make-parser
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
           :make-ll-parser
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

(defun make-parser (http &key header-callback body-callback finish-callback store-body)
  (let* ((header-value-collector nil)
         (current-len 0)
         (completep nil)
         (parser (make-ll-parser))
         callbacks)
    (with-collectors (headers)
      (setq callbacks
            (make-parser-callbacks
             :status (and (http-response-p http)
                          (lambda (parser data start end)
                            (setf (http-status http)
                                  (parser-status-code parser))
                            (setf (http-status-text http)
                                  (babel:octets-to-string data :start start :end end))))
             :header-field (and header-callback
                                (lambda (parser data start end)
                                  (declare (ignore parser))
                                  (when header-value-collector
                                    (let ((header-value
                                            (byte-vector-subseqs-to-string (funcall header-value-collector)
                                                                           current-len)))
                                      (headers
                                       (if (number-string-p header-value)
                                           (read-from-string header-value)
                                           header-value))))
                                  (setq header-value-collector (make-collector))
                                  (setq current-len 0)
                                  (headers (intern (ascii-octets-to-upper-string data :start start :end end)
                                                   :keyword))))
             :header-value (and header-callback
                                (lambda (parser data start end)
                                  (declare (ignore parser))
                                  (incf current-len (- end start))
                                  (funcall header-value-collector (make-byte-vector-subseq data start end))))
             :headers-complete (and header-callback
                                    (lambda (parser)
                                      (setf (http-version http)
                                            (+ (parser-http-major parser)
                                               (/ (parser-http-minor parser) 10)))
                                      (setf (http-method http) (parser-method parser))

                                      ;; collecting the last header-value buffer.
                                      (headers (byte-vector-subseqs-to-string (funcall header-value-collector)
                                                                              current-len))
                                      (setf (http-headers http) headers)
                                      (funcall header-callback headers)))
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
    (return-from make-parser
      (lambda (data)
        (cond
          ((eql data :eof)
           (when finish-callback
             (funcall finish-callback)))
          (T (http-parse parser callbacks data)
             (when (and completep finish-callback)
               (funcall finish-callback))))
        (values http t completep)))))
