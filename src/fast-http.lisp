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
  (:import-from :alexandria
                :named-lambda)
  (:export :make-parser
           :http
           :http-request
           :http-response
           :make-http
           :make-http-request
           :make-http-response
           :http-version
           :http-headers
           :http-body
           :http-method
           :http-resource

           ;; Low-level parser API
           :http-parse
           :ll-parser
           :ll-callbacks
           :make-ll-parser
           :make-ll-callbacks
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
  "Base structure class extended by HTTP-REQUEST and HTTP-RESPONSE."
  version
  headers
  store-body
  body)

(defstruct (http-request (:include http)
                         (:conc-name #.(string :HTTP-)))
  "Structure class holds values specific to an HTTP request (method, resource)."
  method
  resource)

(defstruct (http-response (:include http)
                          (:conc-name #.(string :HTTP-)))
  "Structure class holds values specific to an HTTP response (status, status-text)"
  status
  status-text)

(defun make-parser (http &key header-callback body-callback finish-callback store-body)
  "Returns a lambda function that takes a simple-byte-vector and parses it as an HTTP request/response."
  (declare (optimize (speed 3) (safety 2)))
  (let* ((header-value-collector nil)
         (current-len 0)
         (header-complete-p nil)
         (completep nil)

         (parsing-transfer-encoding-p nil)
         (chunked nil)
         (parsing-content-length-p nil)
         (content-length nil)
         (read-body-length 0)

         (responsep (http-response-p http))
         (parser (make-ll-parser :type (if responsep :response :request)))
         callbacks)
    (with-collectors (headers body-bytes)
      (setq callbacks
            (make-ll-callbacks
             :status (and responsep
                          (named-lambda status-cb (parser data start end)
                            (declare (type simple-byte-vector data))
                            (setf (http-status http)
                                  (parser-status-code parser))
                            (setf (http-status-text http)
                                  (babel:octets-to-string data :start start :end end))))
             :header-field (and (or header-callback body-callback)
                                (named-lambda header-field-cb (parser data start end)
                                  (declare (ignore parser)
                                           (type simple-byte-vector data))
                                  (when header-value-collector
                                    ;; Collect the previous header-value
                                    (let* ((header-value
                                             (byte-vector-subseqs-to-string
                                              (funcall (the function header-value-collector))
                                              current-len))
                                           (header-value
                                             (if (number-string-p header-value)
                                                 (read-from-string header-value)
                                                 header-value)))
                                      (cond
                                        (parsing-transfer-encoding-p
                                         (when (string= header-value "chunked")
                                           (setq chunked t))
                                         (setq parsing-transfer-encoding-p nil))
                                        (parsing-content-length-p
                                         (setq content-length header-value
                                               parsing-content-length-p nil)))
                                      (headers header-value)))
                                  (setq header-value-collector (make-collector))
                                  (setq current-len 0)

                                  ;; Collect the header-field
                                  (let ((header-field
                                          (intern (ascii-octets-to-upper-string data :start start :end end)
                                                  :keyword)))
                                    (case header-field
                                      (:transfer-encoding
                                       (setq parsing-transfer-encoding-p t))
                                      (:content-length
                                       (setq parsing-content-length-p t)))
                                    (headers header-field))))
             :header-value (and header-callback
                                (named-lambda header-value-cb (parser data start end)
                                  (declare (ignore parser)
                                           (type simple-byte-vector data))
                                  (incf current-len (- end start))
                                  (funcall (the function header-value-collector)
                                           (make-byte-vector-subseq data start end))))
             :headers-complete (labels ((headers-complete-cb-with-callback (parser)
                                          (headers-complete-cb parser)
                                          ;; collecting the last header-value buffer.
                                          (headers
                                           (byte-vector-subseqs-to-string
                                            (funcall (the function header-value-collector))
                                            current-len))
                                          (setf (http-headers http) headers)
                                          (funcall (the function header-callback) headers))
                                        (headers-complete-cb (parser)
                                          (setq header-complete-p t)
                                          (setf (http-version http)
                                                (+ (parser-http-major parser)
                                                   (/ (parser-http-minor parser) 10)))
                                          (setf (http-method http) (parser-method parser))))
                                 (if header-callback
                                     #'headers-complete-cb-with-callback
                                     #'headers-complete-cb))
             :url (named-lambda url-cb (parser data start end)
                    (declare (ignore parser)
                             (type simple-byte-vector data))
                    (setf (http-resource http)
                          (babel:octets-to-string data :start start :end end)))
             :body (and body-callback
                        (named-lambda body-cb (parser data start end)
                          (declare (ignore parser)
                                   (type simple-byte-vector data))
                          (incf read-body-length (- end start))
                          (cond
                            (chunked
                             (funcall (the function body-callback) body-bytes)
                             ;; Collect this subseq for :message-complete callback
                             (when (http-store-body http)
                               (body-bytes (make-byte-vector-subseq data start end)))
                             (funcall body-callback (subseq data start end)))
                            (content-length
                             (body-bytes (make-byte-vector-subseq data start end))
                             (when (<= content-length read-body-length)
                               (let ((body (byte-vector-subseqs-to-byte-vector body-bytes
                                                                               read-body-length)))
                                 (when (http-store-body http)
                                   (setf (http-body http) body))
                                 (funcall body-callback body))))
                            (T
                             ;; No Content-Length, no chunking, probably a request with no body
                             ))))
             :message-complete (named-lambda message-complete-cb (parser)
                                 (declare (ignore parser))
                                 (when (and (http-store-body http)
                                            (null (http-body http)))
                                   (setf (http-body http)
                                         (byte-vector-subseqs-to-byte-vector body-bytes
                                                                             read-body-length)))
                                 (setq completep t)))))
    (setf (http-store-body http) store-body)
    (return-from make-parser
      (named-lambda http-parser-execute (data)
        (cond
          ((eql data :eof)
           (when finish-callback
             (funcall finish-callback)))
          (T (http-parse parser callbacks (the simple-byte-vector data))
             (when (and completep finish-callback)
               (funcall finish-callback))))
        (values http header-complete-p completep)))))
