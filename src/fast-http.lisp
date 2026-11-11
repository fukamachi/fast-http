(in-package :cl-user)
(defpackage fast-http
  (:use :cl
        :fast-http.http
        :fast-http.parser
        :fast-http.multipart-parser
        :fast-http.byte-vector
        :fast-http.error
        :xsubseq)
  (:import-from :fast-http.http
                :+state-body+)
  (:import-from :fast-http.parser
                :parse-header-value-parameters)
  (:import-from :fast-http.multipart-parser
                :+body-done+)
  (:import-from :fast-http.util
                :defun-careful)
  (:import-from :smart-buffer
                :make-smart-buffer
                :write-to-buffer
                :finalize-buffer)
  (:export :make-parser
           :*max-header-line-bytes*
           :http-request
           :http-response
           :make-http-request
           :make-http-response
           :http-request-p
           :http-response-p
           :make-callbacks
           :http-version
           :http-major-version
           :http-minor-version
           :http-method
           :http-resource
           :http-status
           :http-status-text
           :http-content-length
           :http-chunked-p
           :http-upgrade-p
           :http-headers

           ;; multipart parser
           :make-multipart-parser

           ;; Low-level parser API
           :http
           :http-p
           :make-http
           :parse-request
           :parse-response

           :http-multipart-parse
           :ll-multipart-parser
           :make-ll-multipart-parser

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
           :obsolete-line-folding
           :invalid-content-length
           :duplicate-content-length
           :invalid-chunk-size
           :duplicate-transfer-encoding
           :conflicting-content-length-and-transfer-encoding
           :invalid-constant
           :invalid-internal-state
           :strict-error
           :paused-error
           :unknown-error
           :parser-poisoned

           :http-error-p

           :multipart-parsing-error
           :invalid-multipart-body
           :invalid-boundary

           :header-value-parsing-error
           :invalid-header-value
           :invalid-parameter-key
           :invalid-parameter-value))
(in-package :fast-http)

(defparameter *max-header-line-bytes* 65536
  "Maximum bytes buffered for one incomplete header or request/status line.

Past this bound make-parser signals header-overflow rather than buffering, so a
line that never terminates cannot grow the buffer until the heap is exhausted.")

;; The incomplete line MAKE-PARSER keeps between calls, parsed again together
;; with the next call's data.
(defstruct (kept-line (:constructor make-kept-line ()))
  (octets (make-array 0 :element-type '(unsigned-byte 8)) :type simple-byte-vector)
  (length 0 :type fixnum))

(defun kept-line-empty-p (kept)
  (zerop (kept-line-length kept)))

(defun kept-line-add (kept data start end)
  "Append DATA[START, END) to KEPT."
  (declare (type simple-byte-vector data)
           (type fixnum start end))
  (let* ((old-length (kept-line-length kept))
         (new-length (+ old-length (- end start))))
    (when (< (length (kept-line-octets kept)) new-length)
      ;; Grow geometrically so a line arriving in small pieces is copied O(1)
      ;; times per byte on average.
      (let ((grown (make-array (max new-length (* 2 (length (kept-line-octets kept))))
                               :element-type '(unsigned-byte 8))))
        (replace grown (kept-line-octets kept) :end2 old-length)
        (setf (kept-line-octets kept) grown)))
    (replace (kept-line-octets kept) data :start1 old-length :start2 start :end2 end)
    (setf (kept-line-length kept) new-length)))

(defun kept-line-take (kept data start end)
  "Return a new vector of the kept bytes followed by DATA[START, END), and empty KEPT.
The vector is new because callbacks may hold on to the data they are given."
  (declare (type simple-byte-vector data)
           (type fixnum start end))
  (let* ((kept-length (kept-line-length kept))
         (octets (make-array (+ kept-length (- end start)) :element-type '(unsigned-byte 8))))
    (replace octets (kept-line-octets kept) :end2 kept-length)
    (replace octets data :start1 kept-length :start2 start :end2 end)
    (setf (kept-line-octets kept) (make-array 0 :element-type '(unsigned-byte 8))
          (kept-line-length kept) 0)
    octets))

(defun kept-line-awaits-end-p (kept http)
  "True when KEPT holds part of a request/status or header line that no byte
seen so far can end. Such a line ends only at a LF; the byte after a LF is
needed too (it tells whether the next line folds into this one), so a kept
line ending in LF is ready to be parsed again."
  (and (not (kept-line-empty-p kept))
       (let ((state (http-state http)))
         (or (= state +state-first-line+)
             (= state +state-headers+)
             (= state +state-trailing-headers+)))
       (/= (aref (kept-line-octets kept) (1- (kept-line-length kept))) +lf+)))

(defun-careful make-parser (http &key first-line-callback header-callback body-callback finish-callback (head-request nil))
  (declare (type http http))
  (let (callbacks

        (parse-fn (etypecase http
                    (http-request #'parse-request)
                    (http-response #'parse-response)))

        (headers nil)

        (header-value-buffer nil)
        parsing-header-field
        (kept-line (make-kept-line))

        header-complete-p
        completedp)
    (flet ((collect-prev-header-value ()
             (when header-value-buffer
               (let ((header-value
                       (locally (declare (optimize (speed 3) (safety 0)))
                         (coerce-to-string
                          (the (or octets-concatenated-xsubseqs
                                   octets-xsubseq)
                               header-value-buffer)))))
                 (if (string= parsing-header-field "set-cookie")
                     (push header-value (gethash "set-cookie" headers))
                     (multiple-value-bind (previous-value existp)
                         (gethash (the simple-string parsing-header-field) headers)
                       (setf (gethash (the simple-string parsing-header-field) headers)
                             (if existp
                                 (if (simple-string-p previous-value)
                                     (concatenate 'string (the simple-string previous-value) ", " header-value)
                                     (format nil "~A, ~A" previous-value header-value))
                                 header-value))))))))
      (setq callbacks
            (make-callbacks
             :message-begin (lambda (http)
                              (declare (ignore http))
                              (setq headers (make-hash-table :test 'equal)
                                    header-complete-p nil
                                    completedp nil))
             :url (lambda (http data start end)
                    (declare (type simple-byte-vector data)
                             (type pointer start end))
                    (setf (http-resource http)
                          (ascii-octets-to-string data :start start :end end)))
             :status (lambda (http data start end)
                       (declare (type simple-byte-vector data)
                                (type pointer start end))
                       (setf (http-status-text http)
                             (ascii-octets-to-string data :start start :end end)))
             :first-line (and first-line-callback
                              (lambda (http)
                                (declare (ignore http))
                                (funcall (the function first-line-callback))))
             :header-field (lambda (http data start end)
                             (declare (ignore http)
                                      (type simple-byte-vector data)
                                      (type pointer start end))
                             (collect-prev-header-value)
                             (setq header-value-buffer (make-concatenated-xsubseqs))
                             (setq parsing-header-field
                                   (ascii-octets-to-lower-string data :start start :end end)))
             :header-value (lambda (http data start end)
                             (declare (ignore http)
                                      (type simple-byte-vector data)
                                      (type pointer start end))
                             (xnconcf header-value-buffer
                                      (xsubseq (subseq (the simple-byte-vector data) start end) 0)))
             :headers-complete (lambda (http)
                                 (collect-prev-header-value)
                                 (setq header-value-buffer nil)
                                 (when (gethash "set-cookie" headers)
                                   (setf (gethash "set-cookie" headers)
                                         (nreverse (gethash "set-cookie" headers))))
                                 (setf (http-headers http) headers)
                                 (when header-callback
                                   (funcall (the function header-callback) headers))
                                 (when (and (not (http-chunked-p http))
                                            (not (numberp (http-content-length http))))
                                   (setq completedp t))
                                 (setq header-complete-p t))
             :body (and body-callback
                        (lambda (http data start end)
                          (declare (ignore http)
                                   (type simple-byte-vector data)
                                   (type pointer start end))
                          (funcall (the function body-callback)
                                   data start end)))
             :message-complete (lambda (http)
                                 (declare (ignore http))
                                 (collect-prev-header-value)
                                 (when finish-callback
                                   (funcall (the function finish-callback)))
                                 (setq completedp t)))))

    (flet ((keep-line (data start end)
             (kept-line-add kept-line data start end)
             (when (< *max-header-line-bytes* (kept-line-length kept-line))
               (setf (http-error-p http) t)
               (error 'header-overflow))))
      (lambda (data &key (start 0) end)
        (declare (optimize (speed 3) (safety 2)))
        (when (http-error-p http)
          (error 'parser-poisoned))
        (cond
          ((eql data :eof)
           (setq completedp t)
           (when finish-callback
             (funcall (the function finish-callback))))
          (T
           (locally (declare (type simple-byte-vector data)
                             (type pointer start))
             (check-type end (or null pointer))
             (let ((end (or end (length data))))
               (declare (type pointer end))
               (cond
                 ((and (kept-line-awaits-end-p kept-line http)
                       (not (find +lf+ data :start start :end end)))
                  ;; Parsing the kept line again could only stop where it
                  ;; stopped last time, so a long line fed in small pieces
                  ;; would cost O(N^2). Wait for a byte that can end it; an
                  ;; invalid byte in the line is reported then.
                  (keep-line data start end))
                 (T
                  (unless (kept-line-empty-p kept-line)
                    (setq data (kept-line-take kept-line data start end)
                          start 0
                          end (length data)))
                  (setf (http-mark http) start)
                  (handler-case
                      (funcall parse-fn http callbacks (the simple-byte-vector data) :start start :end end :head-request head-request)
                    (eof ()
                      ;; The mark advances per completed line, so this tail
                      ;; is one incomplete line; bounding it bounds a single
                      ;; header/request line.
                      (keep-line data (http-mark http) end))
                    (parsing-error (e)
                      (setf (http-error-p http) t)
                      (error e)))))))))
        (values http header-complete-p completedp)))))

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
          (body-buffer (make-smart-buffer))
          callbacks)
      (flet ((collect-prev-header-value ()
               (when header-value-buffer
                 (let ((header-value
                         (babel:octets-to-string
                          (coerce-to-sequence header-value-buffer))))
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
              (make-callbacks
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
                                        (xsubseq (subseq data start end) 0)))
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
                                         body-buffer (make-smart-buffer)
                                         header-value-buffer nil))
               :body (lambda (parser data start end)
                       (declare (ignore parser))
                       (write-to-buffer body-buffer data start end)))))
      (lambda (data)
        (http-multipart-parse parser callbacks data)
        (= (ll-multipart-parser-state parser) +body-done+)))))
