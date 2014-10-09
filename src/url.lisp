(in-package :cl-user)
(defpackage fast-http.url
  (:use :cl
        :fast-http.byte-vector
        :fast-http.variables)
  (:import-from :fast-http.util
                :casev)
  (:export :parse-url-char))
(in-package :fast-http.url)

(declaim (inline userinfo-byte-char-p))
(defun userinfo-byte-char-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (or (alphanumeric-byte-char-p byte)
      (mark-byte-char-p byte)
      (= byte #.(char-code #\%))
      (= byte #.(char-code #\;))
      (= byte #.(char-code #\:))
      (= byte #.(char-code #\&))
      (= byte #.(char-code #\=))
      (= byte #.(char-code #\+))
      (= byte #.(char-code #\$))
      (= byte #.(char-code #\,))))

(declaim (inline url-byte-char-p))
(defun url-byte-char-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (and (<= #.(char-code #\!) byte #.(char-code #\~))
       (not (= byte #.(char-code #\?)))
       (not (= byte #.(char-code #\#)))))

(declaim (ftype (function (fixnum (unsigned-byte 8)) fixnum) parse-url-char))
(defun parse-url-char (state byte)
  (declare (optimize (speed 3) (safety 0)))
  (or
   (cond
     ((or (= byte +space+)
          (= byte +cr+)
          (= byte +lf+))
      +state-dead+)
     ;; when strict mode
     ((or (= byte +tab+)
          (= byte +page+))
      +state-dead+)
     (T
      (casev state
        (+state-req-spaces-before-url+
         ;; Proxied requests are followed by scheme of an absolute URI (alpha).
         ;; All methods except CONNECT are followed by '/' or '*'.
         (cond
           ((or (= byte #.(char-code #\/))
                (= byte #.(char-code #\*)))
            +state-req-path+)
           ((alpha-byte-char-p byte)
            +state-req-schema+)))
        (+state-req-schema+
         (cond
           ((alpha-byte-char-p byte) state)
           ((= byte #.(char-code #\:))
            +state-req-schema-slash+)))
        (+state-req-schema-slash+
         (when (= byte #.(char-code #\/))
           +state-req-schema-slash-slash+))
        (+state-req-schema-slash-slash+
         (when (= byte #.(char-code #\/))
           +state-req-server-start+))
        ((+state-req-server-with-at+
          +state-req-server-start+
          +state-req-server+)
         (cond
           ((and (= state +state-req-server-with-at+)
                 (= byte #.(char-code #\@)))
            +state-dead+)
           ((= byte #.(char-code #\/))
            +state-req-path+)
           ((= byte #.(char-code #\?))
            +state-req-query-string-start+)
           ((= byte #.(char-code #\@))
            +state-req-server-with-at+)
           ((or (userinfo-byte-char-p byte)
                (= byte #.(char-code #\[))
                (= byte #.(char-code #\])))
            +state-req-server+)))
        (+state-req-path+
         (cond
           ((= byte #.(char-code #\?))
            +state-req-query-string-start+)
           ((= byte #.(char-code #\#))
            +state-req-fragment-start+)
           ((<= #.(char-code #\!) byte #.(char-code #\~))
            state)
           ;; utf-8 path
           ((<= 128 byte) state)))
        ((+state-req-query-string-start+
          +state-req-query-string+)
         (cond
           ((= byte #.(char-code #\?))
            ;; allow extra '?' in query string
            +state-req-query-string+)
           ((= byte #.(char-code #\#))
            +state-req-fragment-start+)
           ((<= #.(char-code #\!) byte #.(char-code #\~))
            +state-req-query-string+)))
        (+state-req-fragment-start+
         (cond
           ((= byte #.(char-code #\?))
            +state-req-fragment+)
           ((= byte #.(char-code #\#))
            state)
           ((<= #.(char-code #\!) byte #.(char-code #\~))
            +state-req-fragment+)))
        (+state-req-fragment+
         (when (<= #.(char-code #\!) byte #.(char-code #\~))
           state)))))
   +state-dead+))
