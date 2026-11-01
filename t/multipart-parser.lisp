(in-package :cl-user)
(defpackage fast-http-test.multipart-parser
  (:use :cl
        :fast-http
        :fast-http.multipart-parser
        :fast-http.parser
        :fast-http-test.test-utils
        :xsubseq
        :prove)
  (:import-from :cl-utilities
                :collecting
                :collect))
(in-package :fast-http-test.multipart-parser)

(syntax:use-syntax :interpol)

(plan nil)

(defun test-ll-parser (boundary body expected &optional description)
  (let ((parser (make-ll-multipart-parser :boundary boundary))
        results
        headers)
    (http-multipart-parse parser
                          (make-callbacks
                           :header-field (lambda (parser data start end)
                                           (declare (ignore parser))
                                           (push (cons (babel:octets-to-string data :start start :end end)
                                                       nil)
                                                 headers))
                           :header-value (lambda (parser data start end)
                                           (declare (ignore parser))
                                           (setf (cdr (car headers))
                                                 (append (cdr (car headers))
                                                         (list (babel:octets-to-string data :start start :end end)))))
                           :body (lambda (parser data start end)
                                   (declare (ignore parser))
                                   (push
                                    (list :headers
                                          (loop for (key . values) in (nreverse headers)
                                                append (list key (apply #'concatenate 'string values)))
                                          :body
                                          (babel:octets-to-string data :start start :end end))
                                    results)
                                   (setf headers nil)))
                          body)
    (is (nreverse results) expected description)))

(test-ll-parser "AaB03x"
                (bv (str #?"--AaB03x\r\n"
                         #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                         #?"\r\n"
                         #?"Joe Blow\r\nalmost tricked you!\r\n"
                         #?"--AaB03x\r\n"
                         #?"Content-Disposition: form-data; name=\"pics\"; filename=\"file1.txt\"\r\n"
                         #?"Content-Type: text/plain\r\n"
                         #?"\r\n"
                         #?"... contents of file1.txt ...\r\r\n"
                         #?"--AaB03x--\r\n"))
                '((:headers ("Content-Disposition" "form-data; name=\"field1\"")
                   :body #?"Joe Blow\r\nalmost tricked you!")
                  (:headers ("Content-Disposition" "form-data; name=\"pics\"; filename=\"file1.txt\""
                             "Content-Type" "text/plain")
                   :body #?"... contents of file1.txt ...\r"))
                "rfc1867")

(let ((big-content (make-string (* 1024 3) :initial-element #\a)))
  (test-ll-parser "---------------------------186454651713519341951581030105"
                  (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                           #?"Content-Disposition: form-data; name=\"file1\"; filename=\"random.png\"\r\n"
                           #?"Content-Type: image/png\r\n"
                           #?"\r\n"
                           big-content #?"\r\n"
                           #?"-----------------------------186454651713519341951581030105\r\n"
                           #?"Content-Disposition: form-data; name=\"file2\"; filename=\"random.png\"\r\n"
                           #?"Content-Type: image/png\r\n"
                           #?"\r\n"
                           big-content big-content #?"\r\n"
                           #?"-----------------------------186454651713519341951581030105--\r\n"))
                  `((:headers ("Content-Disposition" "form-data; name=\"file1\"; filename=\"random.png\""
                                                     "Content-Type" "image/png")
                     :body ,big-content)
                    (:headers ("Content-Disposition" "form-data; name=\"file2\"; filename=\"random.png\""
                                                     "Content-Type" "image/png")
                     :body ,(concatenate 'string big-content big-content)))
                  "big file"))

(test-ll-parser "---------------------------186454651713519341951581030105"
                (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                         #?"Content-Disposition: form-data; name=\"name\"\r\n"
                         #?"Content-Type: text/plain\r\n"
                         #?"\r\n"
                         #?"深町英太郎\r\n"
                         #?"-----------------------------186454651713519341951581030105\r\n"
                         #?"Content-Disposition: form-data; name=\"introduce\"\r\n"
                         #?"Content-Type: text/plain\r\n"
                         #?"\r\n"
                         #?"Common Lispが好きです。好きな関数はconsです。\r\n"
                         #?"-----------------------------186454651713519341951581030105--\r\n"))
                '((:headers ("Content-Disposition" "form-data; name=\"name\""
                             "Content-Type" "text/plain")
                   :body "深町英太郎")
                  (:headers ("Content-Disposition" "form-data; name=\"introduce\""
                             "Content-Type" "text/plain")
                   :body "Common Lispが好きです。好きな関数はconsです。"))
                "UTF-8")

(test-ll-parser "---------------------------186454651713519341951581030105"
                (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                         #?"Content-Disposition: form-data;\r\n"
                         #?"\tname=\"file1\"; filename=\"random.png\"\r\n"
                         #?"Content-Type: image/png\r\n"
                         #?"\r\n"
                         #?"abc\r\n"
                         #?"-----------------------------186454651713519341951581030105\r\n"
                         #?"Content-Disposition: form-data;\r\n"
                         #?" name=\"text\"\r\n"
                         #?"\r\n"
                         #?"Test text\n with\r\n ümläuts!\r\n"
                         #?"-----------------------------186454651713519341951581030105--\r\n"))
                '((:headers ("Content-Disposition" #?"form-data;\tname=\"file1\"; filename=\"random.png\""
                             "Content-Type" "image/png")
                   :body "abc")
                  (:headers ("Content-Disposition" "form-data; name=\"text\"")
                   :body #?"Test text\n with\r\n ümläuts!"))
                "multiline header value")

(let ((parser (make-ll-multipart-parser :boundary "AaB03x"))
      (callbacks (make-callbacks
                  :body (lambda (parser data start end)
                          (declare (ignore parser))
                          (princ (babel:octets-to-string data :start start :end end))))))
  (flet ((parse (body)
           (http-multipart-parse parser callbacks body)))
    (is-print (parse (bv (str #?"--AaB03x\r\n"
                              #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                              #?"\r\n"
                              #?"Joe Blow\r\nalmost tricked you!\r\n")))
              #?"Joe Blow\r\nalmost tricked you!")
    (is-print (parse (bv #?"--AaB03x\r\n")) "")))

(let ((parser (make-ll-multipart-parser :boundary "AaB03x"))
      (callbacks (make-callbacks
                  :body (lambda (parser data start end)
                          (declare (ignore parser))
                          (princ (babel:octets-to-string data :start start :end end))))))
  (flet ((parse (body)
           (http-multipart-parse parser callbacks body)))
    (is-print (parse (bv (str #?"--AaB03x\r\n"
                              #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                              #?"\r\n"
                              #?"Joe Blow\r\nalmost tricked you!\r\n")))
              #?"Joe Blow\r\nalmost tricked you!")
    (is-print (parse (bv #?"--Aa")) "")
    (is-print (parse (bv #?"B03x\r\n"))
              #?"")))

(let ((parser (make-ll-multipart-parser :boundary "AaB03x"))
      (callbacks (make-callbacks
                  :body (lambda (parser data start end)
                          (declare (ignore parser))
                          (princ (babel:octets-to-string data :start start :end end))))))
  (flet ((parse (body)
           (http-multipart-parse parser callbacks body)))
    (is-print (parse (bv (str #?"--AaB03x\r\n"
                              #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                              #?"\r\n"
                              #?"Joe Blow\r\nalmost tricked you!\r\n")))
              #?"Joe Blow\r\nalmost tricked you!")
    (is-print (parse (bv #?"--Aa")) "")
    (is-print (parse (bv #?"BbCc\r\n"))
              #?"\r\n--AaBbCc")))

(let ((parser (make-ll-multipart-parser :boundary "AaB03x"))
      (callbacks (make-callbacks
                  :body (lambda (parser data start end)
                          (declare (ignore parser))
                          (princ (babel:octets-to-string data :start start :end end))))))
  (flet ((parse (body)
           (http-multipart-parse parser callbacks body)))
    (is-print (parse (bv (str #?"--AaB03x\r\n"
                              #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                              #?"\r\n"
                              #?"Joe Blow\r\nalmost tricked you!\r\n")))
              #?"Joe Blow\r\nalmost tricked you!")
    (is-print (parse (bv "--Aa")) "")
    (is-print (parse (bv "B03x")) "")
    (is-print (parse (bv "C")) #?"\r\n--AaB03xC")))


(defun slurp-stream (stream)
  (with-xsubseqs (xsub)
    (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
      (loop for bytes-read = (read-sequence buffer stream)
            do (xnconcf xsub (xsubseq buffer 0 bytes-read))
            while (= bytes-read 1024)))))

(defun test-parser (content-type data expected &optional description)
  (let ((got (collecting
               (let ((parser
                       (make-multipart-parser content-type
                                              (lambda (field-name headers field-meta body)
                                                (setf body (slurp-stream body))
                                                (collect (list field-name
                                                               headers
                                                               field-meta
                                                               (babel:octets-to-string body)))))))
                 (funcall parser data)))))
    (flet ((hash-table-equal (hash plist desc)
             (subtest desc
               (maphash (lambda (k v)
                          (is v (getf plist (intern (string-upcase k) :keyword)) k))
                        hash))))
      (subtest (or description "")
        (loop for got in got
              for expected in expected
              do (is (first got) (first expected) "field-name")
                 (hash-table-equal (second got) (second expected) "headers")
                 (hash-table-equal (third got) (third expected) "field-meta")
                 (is (fourth got) (fourth expected) "body"))))))

(test-parser "multipart/form-data; boundary=AaB03x"
             (bv (str #?"--AaB03x\r\n"
                      #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                      #?"\r\n"
                      #?"Joe Blow\r\nalmost tricked you!\r\n"
                      #?"--AaB03x\r\n"
                      #?"Content-Disposition: form-data; name=\"pics\"; filename=\"file1.txt\"\r\n"
                      #?"Content-Type: text/plain\r\n"
                      #?"\r\n"
                      #?"... contents of file1.txt ...\r\r\n"
                      #?"--AaB03x--\r\n"))
             '(("field1"
                (:content-disposition "form-data; name=\"field1\"")
                (:name "field1")
                #?"Joe Blow\r\nalmost tricked you!")
               ("pics"
                (:content-disposition "form-data; name=\"pics\"; filename=\"file1.txt\""
                 :content-type "text/plain")
                (:name "pics"
                 :filename "file1.txt")
                #?"... contents of file1.txt ...\r"))
             "rfc1867")

(let ((big-content (make-string (* 1024 3) :initial-element #\a)))
  (test-parser "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
               (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                        #?"Content-Disposition: form-data; name=\"file1\"; filename=\"random.png\"\r\n"
                        #?"Content-Type: image/png\r\n"
                        #?"\r\n"
                        big-content #?"\r\n"
                        #?"-----------------------------186454651713519341951581030105\r\n"
                        #?"Content-Disposition: form-data; name=\"file2\"; filename=\"random.png\"\r\n"
                        #?"Content-Type: image/png\r\n"
                        #?"\r\n"
                        big-content big-content #?"\r\n"
                        #?"-----------------------------186454651713519341951581030105--\r\n"))
               `(("file1"
                  (:content-disposition "form-data; name=\"file1\"; filename=\"random.png\""
                   :content-type "image/png")
                  (:name "file1" :filename "random.png")
                  ,big-content)
                 ("file2"
                  (:content-disposition "form-data; name=\"file2\"; filename=\"random.png\""
                   :content-type "image/png")
                  (:name "file2" :filename "random.png")
                  ,(concatenate 'string big-content big-content)))
               "big file"))

(test-parser "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
             (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                      #?"Content-Disposition: form-data; name=\"name\"\r\n"
                      #?"Content-Type: text/plain\r\n"
                      #?"\r\n"
                      #?"深町英太郎\r\n"
                      #?"-----------------------------186454651713519341951581030105\r\n"
                      #?"Content-Disposition: form-data; name=\"introduce\"\r\n"
                      #?"Content-Type: text/plain\r\n"
                      #?"\r\n"
                      #?"Common Lispが好きです。好きな関数はconsです。\r\n"
                      #?"-----------------------------186454651713519341951581030105--\r\n"))
             '(("name"
                (:content-disposition "form-data; name=\"name\""
                 :content-type "text/plain")
                (:name "name")
                "深町英太郎")
               ("introduce"
                (:content-disposition "form-data; name=\"introduce\""
                 :content-type "text/plain")
                (:name "introduce")
                "Common Lispが好きです。好きな関数はconsです。"))
             "UTF-8")

(test-parser "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
             (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                      #?"Content-Disposition: form-data;\r\n"
                      #?"\tname=\"file1\"; filename=\"random.png\"\r\n"
                      #?"Content-Type: image/png\r\n"
                      #?"\r\n"
                      #?"abc\r\n"
                      #?"-----------------------------186454651713519341951581030105\r\n"
                      #?"Content-Disposition: form-data;\r\n"
                      #?" name=\"text\"\r\n"
                      #?"\r\n"
                      #?"Test text\n with\r\n ümläuts!\r\n"
                      #?"-----------------------------186454651713519341951581030105--\r\n"))
             '(("file1"
                (:content-disposition #?"form-data;\tname=\"file1\"; filename=\"random.png\""
                 :content-type "image/png")
                (:name "file1" :filename "random.png")
                "abc")
               ("text"
                (:content-disposition "form-data; name=\"text\"")
                (:name "text")
                #?"Test text\n with\r\n ümläuts!"))
             "multiline header value")


;;
;; Continuous calling

(let* ((got-body nil)
       (parser (make-multipart-parser
                "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
                (lambda (field-name headers field-meta body)
                  (declare (ignore field-name headers field-meta))
                  (setf got-body (slurp-stream body))))))
  (is (funcall parser
               (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                        #?"Content-Disposition: form-data;\r\n"
                        #?"\tname=\"file1\"; filename=\"random.png\"\r\n"
                        #?"Content-Type: image/png\r\n"
                        #?"\r\n"
                        #?"abc\r\n")))
      nil)
  (is got-body nil)
  (is (funcall parser
               (bv #?"-----------------------------186454651713519341951581030105\r\n"))
      nil)
  (is got-body (bv "abc") :test #'equalp))

(let* ((got-body nil)
       (parser (make-multipart-parser
                "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
                (lambda (field-name headers field-meta body)
                  (declare (ignore field-name headers field-meta))
                  (setf got-body (slurp-stream body))))))
  (is (funcall parser
               (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                        #?"Content-Disposition: form-data;\r\n"
                        #?"\tname=\"file1\"; filename=\"random.png\"\r\n"
                        #?"Content-Type: image/png\r\n"
                        #?"\r\n"
                        #?"abc\r\n")))
      nil)
  (is got-body nil)
  (is (funcall parser
               (bv #?"-----------------------------186454651713519341951581030105abc\r\n"))
      nil)
  (is got-body nil)
  (is (funcall parser
               (bv #?"-----------------------------186454651713519341951581030105--"))
      t)
  (is got-body (bv (str #?"abc\r\n"
                        #?"-----------------------------186454651713519341951581030105abc"))
      :test #'equalp))

(let* ((got-body nil)
       (got-headers nil)
       (got-field-meta nil)
       (parser (make-multipart-parser
                "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
                (lambda (field-name headers field-meta body)
                  (declare (ignore field-name))
                  (setf got-headers headers)
                  (setf got-field-meta field-meta)
                  (setf got-body (slurp-stream body))))))
  (is (funcall parser
               (bv (str #?"-----------------------------186454651713519341951581030105\r\n"
                        #?"Content-Disposition: form-data; name=\"file\"; filename=\"黑客与画家(中文版).pdf\"\r\n"
                        #?"Content-Type: application/octet-stream\r\n"
                        #?"\r\n"
                        #?"abc\r\n")))
      nil)
  (is got-headers nil)
  (is got-field-meta nil)
  (is got-body nil)
  (is (funcall parser
               (bv #?"-----------------------------186454651713519341951581030105\r\n"))
      nil)
  (is (gethash "content-disposition" got-headers)
      "form-data; name=\"file\"; filename=\"黑客与画家(中文版).pdf\"")
  (is (gethash "filename" got-field-meta)
      "黑客与画家(中文版).pdf")
  (is got-body (bv "abc") :test #'equalp))


;;
;; Part headers split across calls
;;
;; fast-http.parser::eof is not an ERROR, so it must never escape to the caller:
;; it bypasses (handler-case ... (error ...)) in applications and servers.

(defun parse-multipart-chunks (content-type chunks)
  "Feed CHUNKS to a multipart parser one by one.
Returns a list of (field-name sorted-headers-alist body-string) per part,
or (:condition <type>) if any condition escaped."
  (let ((parts '()))
    (handler-case
        (let ((parser (make-multipart-parser
                       content-type
                       (lambda (field-name headers field-meta body)
                         (declare (ignore field-meta))
                         (push (list field-name
                                     (sort (loop for k being the hash-keys of headers
                                                   using (hash-value v)
                                                 collect (cons k v))
                                           #'string< :key #'car)
                                     (babel:octets-to-string (slurp-stream body)))
                               parts)))))
          (dolist (chunk chunks)
            (funcall parser chunk))
          (nreverse parts))
      (condition (c)
        (list :condition (type-of c))))))

(let* ((content-type "multipart/form-data; boundary=AaB03x")
       (data (bv (str #?"--AaB03x\r\n"
                      #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                      #?"Content-Type: text/plain\r\n"
                      #?"\r\n"
                      #?"hello\r\n"
                      #?"--AaB03x\r\n"
                      #?"Content-Disposition: form-data; name=\"field2\"\r\n"
                      #?"\r\n"
                      #?"world\r\n"
                      #?"--AaB03x--\r\n")))
       (expected (parse-multipart-chunks content-type (list data))))
  (flet ((header-region (from)
           ;; From right after the delimiter line to the final LF of the blank
           ;; line (splitting right after the blank line is a body-side case).
           (let ((start (+ (search (bv #?"--AaB03x\r\n") data :start2 from) 10))
                 (end (+ (search (bv #?"\r\n\r\n") data :start2 from) 4)))
             (values start end)))
         (failing-splits (start end)
           (loop for i from start below end
                 for got = (parse-multipart-chunks content-type
                                                   (list (subseq data 0 i) (subseq data i)))
                 unless (equal got expected)
                   collect (cons i got))))
    (is (length expected) 2 "sanity: two parts in a single call")
    (multiple-value-bind (start end) (header-region 0)
      (is (failing-splits start end) nil
          "splitting inside the first part's headers gives the same result"))
    (multiple-value-bind (start end) (header-region (search (bv "hello") data))
      (is (failing-splits start end) nil
          "splitting inside the second part's headers gives the same result"))))

(let* ((fields 0)
       (parser (make-ll-multipart-parser :boundary "AaB03x"))
       (callbacks (make-callbacks
                   :header-field (lambda (parser data start end)
                                   (declare (ignore parser data start end))
                                   (incf fields)))))
  (http-multipart-parse parser callbacks
                        (bv (str #?"--AaB03x\r\n"
                                 #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                                 #?"Content-Ty")))
  (http-multipart-parse parser callbacks
                        (bv (str #?"pe: text/plain\r\n"
                                 #?"\r\n"
                                 #?"hello\r\n"
                                 #?"--AaB03x--\r\n")))
  (is fields 2 "each header-field callback fires once even when headers are split"))

(is (parse-multipart-chunks "multipart/form-data; boundary=AaB03x"
                            (list (bv (str #?"--AaB03x\r\n"
                                           #?"Content-Disposition: form-da"))))
    nil
    "a body truncated inside part headers signals no condition")

(is-error (let ((parser (make-multipart-parser "multipart/form-data; boundary=AaB03x"
                                               (lambda (&rest args) (declare (ignore args))))))
            (funcall parser (bv #?"--AaB03x\r\nX-Long: "))
            (loop repeat 1024
                  do (funcall parser (bv (make-string 1024 :initial-element #\a)))))
          'fast-http.error:header-overflow
          "an unterminated part header that keeps growing across calls is rejected")


;;
;; The result must not depend on how the input is split into calls

(defun split-failures (content-type data)
  "Split positions where feeding DATA in two calls differs from one call."
  (let ((expected (parse-multipart-chunks content-type (list data))))
    (loop for i from 1 below (length data)
          for got = (parse-multipart-chunks content-type
                                            (list (subseq data 0 i) (subseq data i)))
          unless (equal got expected)
            collect (cons i got))))

(defun byte-chunks (data)
  (loop for i from 0 below (length data)
        collect (subseq data i (1+ i))))

(let ((content-type "multipart/form-data; boundary=AaB03x")
      (data (bv (str #?"--AaB03x\r\n"
                     #?"Content-Disposition: form-data; name=\"field1\"\r\n"
                     #?"\r\n"
                     #?"hello\r\n"
                     #?"--AaB03x\r\n"
                     #?"Content-Disposition: form-data; name=\"field2\"\r\n"
                     #?"\r\n"
                     #?"world\r\n"
                     #?"--AaB03x--\r\n")))
      (expected '(("field1" (("content-disposition" . "form-data; name=\"field1\"")) "hello")
                  ("field2" (("content-disposition" . "form-data; name=\"field2\"")) "world"))))
  (is (parse-multipart-chunks content-type (list data)) expected
      "two parts in a single call")
  (is (split-failures content-type data) nil
      "two parts: every 2-way split gives the same result")
  (is (parse-multipart-chunks content-type (byte-chunks data)) expected
      "two parts: fed one byte at a time"))

;; Bodies containing byte sequences that look like a delimiter but are not.
(let* ((content-type "multipart/form-data; boundary=AaB03x")
       (body-a #?"AAAAA\r\n--AaB03xZZ\r\n--AaB0\r\n-\r\n\r\r\n")
       (body-b #?"\r\n--")
       (data (bv (str #?"--AaB03x\r\n"
                      #?"Content-Disposition: form-data; name=\"a\"\r\n"
                      #?"\r\n"
                      body-a #?"\r\n"
                      #?"--AaB03x\r\n"
                      #?"Content-Disposition: form-data; name=\"b\"\r\n"
                      #?"\r\n"
                      body-b #?"\r\n"
                      #?"--AaB03x--\r\n")))
       (expected `(("a" (("content-disposition" . "form-data; name=\"a\"")) ,body-a)
                   ("b" (("content-disposition" . "form-data; name=\"b\"")) ,body-b))))
  (is (parse-multipart-chunks content-type (list data)) expected
      "delimiter look-alikes in a single call")
  (is (split-failures content-type data) nil
      "delimiter look-alikes: every 2-way split gives the same result")
  (is (parse-multipart-chunks content-type (byte-chunks data)) expected
      "delimiter look-alikes: fed one byte at a time"))

;; A long boundary, as browsers send (cf. a 1-byte chunk inside the boundary).
(let* ((boundary "---------------------------186454651713519341951581030105")
       (content-type (str "multipart/form-data; boundary=" boundary))
       (data (bv (str "--" boundary #?"\r\n"
                      #?"Content-Disposition: form-data; name=\"q\"\r\n"
                      #?"\r\n"
                      #?"Q\r\n--" boundary "BB" #?"\r\n"
                      #?"\r\n"
                      "--" boundary #?"--\r\n")))
       (expected `(("q" (("content-disposition" . "form-data; name=\"q\""))
                        ,(str #?"Q\r\n--" boundary "BB" #?"\r\n")))))
  (is (parse-multipart-chunks content-type (list data)) expected
      "long boundary in a single call")
  (is (split-failures content-type data) nil
      "long boundary: every 2-way split gives the same result")
  (is (parse-multipart-chunks content-type (byte-chunks data)) expected
      "long boundary: fed one byte at a time"))

(finalize)
