(in-package :cl-user)
(defpackage fast-http-test.multipart-parser-bounds
  (:use :cl
        :fast-http
        :fast-http.multipart-parser
        :fast-http.parser
        :fast-http-test.test-utils
        :prove))
(in-package :fast-http-test.multipart-parser-bounds)

;; How much work one client can cause by sending a multipart body in small
;; chunks, and what HTTP-MULTIPART-PARSE reports back for bytes it keeps.

(plan nil)

(defun wire (&rest parts)
  "Bytes of PARTS, strings joined with :CRLF and :LF standing for line ends."
  (bv (apply #'str
             (mapcar (lambda (part)
                       (case part
                         (:crlf (coerce '(#\Return #\Newline) 'string))
                         (:lf (string #\Newline))
                         (otherwise part)))
                     parts))))

(defun parse-events (chunks)
  "Feed CHUNKS to a low-level parser for the boundary AaB03x, one per call.
Returns the (tag . text) header and body events with adjacent body pieces
merged, or (:condition <type>) if any condition escaped."
  (let ((parser (make-ll-multipart-parser :boundary "AaB03x"))
        (events '()))
    (flet ((recorder (tag)
             (lambda (parser data start end)
               (declare (ignore parser))
               (let ((text (map 'string #'code-char (subseq data start end))))
                 (if (and (eq tag :body) (eq (car (first events)) :body))
                     (setf (cdr (first events)) (str (cdr (first events)) text))
                     (push (cons tag text) events))))))
      (let ((callbacks (make-callbacks :header-field (recorder :field)
                                       :header-value (recorder :value)
                                       :body (recorder :body))))
        (handler-case
            (progn
              (dolist (chunk chunks)
                (http-multipart-parse parser callbacks chunk))
              (reverse events))
          (condition (c)
            (list :condition (type-of c))))))))

(defun split-failures (data)
  "Split positions where feeding DATA in two calls differs from one call."
  (let ((expected (parse-events (list data))))
    (loop for i from 1 below (length data)
          for got = (parse-events (list (subseq data 0 i) (subseq data i)))
          unless (equal got expected)
            collect (cons i got))))

(defun byte-chunks (data)
  (loop for i from 0 below (length data)
        collect (subseq data i (1+ i))))

;;
;; With :START greater than 0, positions stay relative to the DATA given and
;; bytes before :START are never delivered to the body callback.

(flet ((padded (&rest parts)
         (apply #'wire "---" (append parts (list "---"))))
       (parse-padded (parser callbacks data)
         (http-multipart-parse parser callbacks data :start 3 :end (- (length data) 3)))
       (fresh ()
         (make-ll-multipart-parser :boundary "b")))
  (let ((data (padded "--b" :crlf "A: x" :crlf :crlf "hello")))
    (is (parse-padded (fresh) (make-callbacks) data) (- (length data) 3)
        ":START is not added twice when no bytes were kept from the previous call"))
  (let ((parser (fresh))
        (callbacks (make-callbacks)))
    (parse-padded parser callbacks (padded "--b" :crlf "A: x" :crlf :crlf "hello" :crlf))
    (let ((data (padded "Z")))
      (is (parse-padded parser callbacks data) (- (length data) 3)
          "the return value with :START is relative to DATA after re-parsing kept bytes")))
  (let* ((parser (fresh))
         (body (make-string-output-stream))
         (callbacks (make-callbacks
                     :body (lambda (parser data start end)
                             (declare (ignore parser))
                             (write-string (map 'string #'code-char (subseq data start end))
                                           body)))))
    (parse-padded parser callbacks (padded "--b" :crlf "A: x" :crlf :crlf "hello"))
    (parse-padded parser callbacks (padded "world"))
    (parse-padded parser callbacks (padded :crlf "--b--"))
    (is (get-output-stream-string body) "helloworld"
        "a body continuing into a call with :START does not include the bytes before it")))

;;
;; A boundary is 1 to 70 characters (RFC 2046). Its length is chosen by the
;; client and an incomplete delimiter is copied and scanned again on every
;; call, so a longer one is rejected when the parser is made.

(flet ((boundary (size)
         (make-string size :initial-element #\b))
       (ignore-part (&rest args)
         (declare (ignore args))))
  (is-error (make-ll-multipart-parser :boundary (boundary 71))
            'fast-http.error:invalid-boundary
            "a 71-character boundary is rejected by the low-level constructor")
  (is-error (make-ll-multipart-parser :boundary "")
            'fast-http.error:invalid-boundary
            "an empty boundary is rejected by the low-level constructor")
  (ok (make-ll-multipart-parser :boundary (boundary 70))
      "a 70-character boundary is accepted by the low-level constructor")
  (is-error (make-multipart-parser (str "multipart/form-data; boundary=" (boundary 71))
                                   #'ignore-part)
            'fast-http.error:invalid-boundary
            "a 71-character boundary signals an error, not NIL, from MAKE-MULTIPART-PARSER")
  (ok (make-multipart-parser (str "multipart/form-data; boundary=" (boundary 70))
                             #'ignore-part)
      "a 70-character boundary is accepted by MAKE-MULTIPART-PARSER"))

;;
;; Bytes kept for the next call are consumed: the return value is the end of
;; this call's data, so a caller resuming from it does not send them twice.

(let ((callbacks (make-callbacks)))
  (flet ((fresh ()
           (make-ll-multipart-parser :boundary "AaB03x")))
    (let ((parser (fresh))
          (data (wire "--AaB03x" :crlf "Content-Di")))
      (is (http-multipart-parse parser callbacks data) (length data)
          "incomplete part headers kept for the next call are consumed")
      (is (http-multipart-parse parser callbacks (bv "s")) 1
          "a byte added to incomplete part headers is consumed"))
    (let ((parser (fresh)))
      (is (http-multipart-parse parser callbacks (bv "--AaB")) 5
          "an incomplete first delimiter kept for the next call is consumed")
      (is (http-multipart-parse parser callbacks (bv "0")) 1
          "a byte added to an incomplete first delimiter is consumed"))
    (let ((parser (fresh))
          (data (wire "--AaB03x" :crlf "A: b" :crlf :crlf "hello" :crlf "--Aa")))
      (is (http-multipart-parse parser callbacks data) (length data)
          "an incomplete delimiter after a body kept for the next call is consumed"))
    (let ((parser (fresh))
          (data (wire "XX--AaB03x" :crlf "Content-DiYY")))
      (is (http-multipart-parse parser callbacks data :start 2 :end (- (length data) 2))
          (- (length data) 2)
          "the end returned for kept bytes is the END given by the caller"))))

;;
;; Part headers are parsed only once the blank line that ends them has arrived.
;; Only CRLF ends a header line, so LF LF does not end the block.

(is (parse-events (list (wire "--AaB03x" :crlf "Bad Header: x" :crlf)))
    nil
    "headers without the blank line are kept, not parsed")
(is (parse-events (list (wire "--AaB03x" :crlf "Bad Header: x" :lf :lf)))
    nil
    "LF LF does not end part headers, so they are not parsed")
(is (parse-events (list (wire "--AaB03x" :crlf "Bad Header: x" :crlf :crlf)))
    '(:condition fast-http.error:invalid-header-token)
    "headers are parsed and rejected once the blank line has arrived")

(let ((data (wire "--AaB03x" :crlf
                  "X-A: b" :lf :lf "c" :crlf
                  :crlf
                  "hello" :crlf
                  "--AaB03x--" :crlf))
      (expected `((:field . "X-A")
                  (:value . ,(format nil "b~c~cc" #\Newline #\Newline))
                  (:body . "hello"))))
  (is (parse-events (list data)) expected
      "LF LF inside a header value in a single call")
  (is (split-failures data) nil
      "LF LF inside a header value: every 2-way split gives the same result")
  (is (parse-events (byte-chunks data)) expected
      "LF LF inside a header value: fed one byte at a time"))

;; The limit is checked before the block is parsed, so which error a block both
;; invalid and too large signals does not depend on how it is split.
(let ((data (wire "--AaB03x" :crlf
                  "Bad Header: "
                  (make-string fast-http.multipart-parser::+max-part-header-bytes+
                               :initial-element #\a)
                  :crlf :crlf)))
  (is (parse-events (list data))
      '(:condition fast-http.error:header-overflow)
      "an invalid block over the limit is rejected as too large in a single call")
  (is (split-failures data) nil
      "an invalid block over the limit: every 2-way split gives the same result")
  (is (parse-events (byte-chunks data))
      '(:condition fast-http.error:header-overflow)
      "an invalid block over the limit: fed one byte at a time"))

;; A part without headers: its block is only the blank line, so it contains no
;; CRLF CRLF of its own.
(let ((data (wire "--AaB03x" :crlf :crlf "nohdr" :crlf "--AaB03x--" :crlf))
      (expected '((:body . "nohdr"))))
  (is (parse-events (list data)) expected
      "a part without headers in a single call")
  (is (split-failures data) nil
      "a part without headers: every 2-way split gives the same result")
  (is (parse-events (byte-chunks data)) expected
      "a part without headers: fed one byte at a time"))

(finalize)
