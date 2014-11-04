(in-package :cl-user)
(defpackage fast-http
  (:use :cl
        :fast-http.http
        :fast-http.parser
        :fast-http.unparser
        :fast-http.body-buffer
        :fast-http.byte-vector
        :fast-http.error
        :xsubseq)
  (:import-from :fast-http.util
                :defun-careful
                :make-collector
                :number-string-p)
  (:import-from :babel
                :octets-to-string)
  (:export :make-parser
           :http
           :make-http
           :make-callbacks
           :http-major-version
           :http-minor-version
           :http-method
           :http-status-code
           :http-content-length
           :http-chunked-p
           :http-upgrade-p

           ;; Low-level parser API
           :parse-request
           :parse-response

           ;; unparser
           :http-unparse

           ;; body-buffer
           :*default-memory-limit*
           :*default-disk-limit*
           :body-buffer-limit-exceeded

           ;; Error
           :fast-http-error

           :callback-error
           :cb-message-begin
           :cb-url
           :cb-first-line
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
           :unknown-error

           :multipart-parsing-error
           :invalid-multipart-body
           :invalid-boundary

           :header-value-parsing-error
           :invalid-header-value
           :invalid-parameter-key
           :invalid-parameter-value))
(in-package :fast-http)

(defun-careful make-parser (http type &key first-line-callback header-callback body-callback finish-callback)
  (declare (type http http))
  (let (callbacks

        (parse-fn (ecase type
                    (:request #'parse-request)
                    (:response #'parse-response)))

        (headers (make-hash-table :test 'equal))

        (header-value-collector (make-collector))
        parsing-header-field
        data-buffer

        completedp)
    (declare (type function header-value-collector))
    (flet ((collect-prev-header-value ()
             (let ((header-values (funcall header-value-collector)))
               (unless header-values
                 (return-from collect-prev-header-value))
               (let* ((len (reduce #'+ header-values :key #'length))
                      (header-value (make-string len)))
                 (loop with current-pos = 0
                       for val of-type (array (unsigned-byte 8) (*)) in header-values
                       do (loop for byte of-type (unsigned-byte 8) across val
                                do (setf (aref header-value current-pos) (code-char byte))
                                   (incf current-pos)))
                 (multiple-value-bind (previous-value existp)
                     (gethash (the simple-string parsing-header-field) headers)
                   (setf (gethash (the simple-string parsing-header-field) headers)
                         (if existp
                             (concatenate 'string (the simple-string previous-value) ", " header-value)
                             (if (number-string-p header-value)
                                 (read-from-string header-value)
                                 header-value))))))
             (setq header-value-collector (make-collector))))
      (setq callbacks
            (make-callbacks
             :first-line (and first-line-callback
                              (lambda (http)
                                (declare (ignore http))
                                (funcall (the function first-line-callback))))
             :header-field (lambda (http data start end)
                             (declare (ignore http)
                                      (type simple-byte-vector data)
                                      (type pointer start end))
                             (collect-prev-header-value)
                             (setq parsing-header-field
                                   (ascii-octets-to-lower-string data :start start :end end)))
             :header-value (lambda (http data start end)
                             (declare (ignore http)
                                      (type simple-byte-vector data)
                                      (type pointer start end))
                             (funcall header-value-collector
                                      (make-array (- end start)
                                                  :element-type '(unsigned-byte 8)
                                                  :displaced-to data
                                                  :displaced-index-offset start)))
             :headers-complete (lambda (http)
                                 (declare (ignore http))
                                 (collect-prev-header-value)
                                 (when header-callback
                                   (funcall (the function header-callback) headers)))
             :body (and body-callback
                        (lambda (http data start end)
                          (declare (ignore http)
                                   (type simple-byte-vector data)
                                   (type pointer start end))
                          (funcall (the function body-callback)
                                   (make-array (- end start)
                                               :element-type '(unsigned-byte 8)
                                               :displaced-to data
                                               :displaced-index-offset start))))
             :message-complete (lambda (http)
                                 (declare (ignore http))
                                 (collect-prev-header-value)
                                 (when finish-callback
                                   (funcall (the function finish-callback)))
                                 (setq completedp t)))))

    (lambda (data &key (start 0) end)
      (declare (optimize (speed 3) (safety 2)))
      (cond
        ((eql data :eof)
         (setq completedp t)
         (when finish-callback
           (funcall (the function finish-callback))))
        (T
         (locally (declare (type simple-byte-vector data)
                           (type pointer start))
           (check-type end (or null pointer))
           (when data-buffer
             ;; XXX: make this efficient
             (setq data (concatenate 'simple-byte-vector data-buffer data))
             (setq data-buffer nil))
           (handler-case
               (funcall parse-fn http callbacks (the simple-byte-vector data) :start start :end end)
             (eof ()
               (setq data-buffer
                     (make-array (- (or end (length data))
                                    (http-mark http))
                                 :element-type '(unsigned-byte 8)
                                 :displaced-to data
                                 :displaced-index-offset (http-mark http)))))))))))

#+todo
(defun find-boundary (content-type)
  (declare (type string content-type))
  (let ((parsing-boundary nil))
    (parse-header-value-parameters content-type
                                   :header-value-callback
                                   (lambda (data start end)
                                     (unless (string= data "multipart/form-data"
                                                      :start1 start :end1 end)
                                       (return-from find-boundary nil)))
                                   :header-parameter-key-callback
                                   (lambda (data start end)
                                     (when (string= data "boundary"
                                                    :start1 start :end1 end)
                                       (setq parsing-boundary t)))
                                   :header-parameter-value-callback
                                   (lambda (data start end)
                                     (when parsing-boundary
                                       (return-from find-boundary (subseq data start end)))))))

#+todo
(defun make-multipart-parser (content-type callback)
  (check-type content-type string)
  (let ((boundary (find-boundary content-type)))
    (unless boundary
      (return-from make-multipart-parser nil))

    (let ((parser (make-ll-multipart-parser :boundary boundary))
          (headers (make-hash-table :test 'equal))
          parsing-content-disposition
          parsing-header-field
          field-meta
          header-value-buffer
          (body-buffer (make-body-buffer))
          callbacks)
      (flet ((collect-prev-header-value ()
               (when header-value-buffer
                 (let ((header-value
                         (coerce-to-string header-value-buffer)))
                   (when parsing-content-disposition
                     (setq field-meta
                           (let (parsing-key
                                 (field-meta (make-hash-table :test 'equal)))
                             (parse-header-value-parameters header-value
                                                            :header-parameter-key-callback
                                                            (lambda (data start end)
                                                              (setq parsing-key
                                                                    (string-downcase (subseq data start end))))
                                                            :header-parameter-value-callback
                                                            (lambda (data start end)
                                                              (setf (gethash parsing-key field-meta)
                                                                    (subseq data start end))))
                             field-meta)))
                   (setf (gethash parsing-header-field headers)
                         header-value)))))
        (setq callbacks
              (make-ll-callbacks
               :header-field (lambda (parser data start end)
                               (declare (ignore parser))
                               (collect-prev-header-value)
                               (setq header-value-buffer (make-concatenated-xsubseqs))

                               (let ((header-name
                                       (ascii-octets-to-lower-string data :start start :end end)))
                                 (setq parsing-content-disposition
                                       (string= header-name "content-disposition"))
                                 (setq parsing-header-field header-name)))
               :header-value (lambda (parser data start end)
                               (declare (ignore parser))
                               (xnconcf header-value-buffer
                                        (xsubseq data start end)))
               :headers-complete (lambda (parser)
                                   (declare (ignore parser))
                                   (collect-prev-header-value))
               :message-complete (lambda (parser)
                                   (declare (ignore parser))
                                   (funcall callback
                                            (gethash "name" field-meta)
                                            headers
                                            field-meta
                                            (finalize-buffer body-buffer))
                                   (setq headers (make-hash-table :test 'equal)
                                         body-buffer (make-body-buffer)
                                         header-value-buffer nil))
               :body (lambda (parser data start end)
                       (declare (ignore parser))
                       (write-to-buffer body-buffer data start end)))))
      (lambda (data)
        (http-multipart-parse parser callbacks data)
        (= (ll-multipart-parser-state parser) +body-done+)))))
