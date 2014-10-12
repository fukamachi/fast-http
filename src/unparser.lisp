(in-package :cl-user)
(defpackage fast-http.unparser
  (:use :cl
        :fast-http.http)
  (:import-from :fast-http.byte-vector
                :+crlf+)
  (:import-from :alexandria
                :if-let)
  (:export :http-unparse))
(in-package :fast-http.unparser)

(defun http-status-line (http)
  (check-type http http-response)
  (format nil "HTTP/~F ~A ~A~C~C"
          (or (http-version http) 1.1)
          (http-status http)
          (or (http-status-text http)
              (status-code-to-text (http-status http)))
          #\Return
          #\Newline))

(defun header-line (header-field header-value)
  (format nil "~:(~A~): ~A~C~C"
          header-field
          header-value
          #\Return
          #\Newline))

(defun http-request-line (http)
  (check-type http http-request)
  (let ((method (http-method http))
        (resource (http-resource http))
        (version (or (http-version http)
                     1.1)))
    (unless resource
      (error "Empty URI resource"))
    (format nil "~:@(~A~) ~A HTTP/~F~C~C"
            method
            resource
            version
            #\Return
            #\Newline)))

(defun write-http-headers (http writer)
  (loop for (k v) on (http-headers http) by #'cddr
        when v
          do (funcall writer (header-line k v)))
  (funcall writer +crlf+))

(defun http-unparse (http writer)
  (etypecase http
    (http-request (funcall writer (http-request-line http)))
    (http-response (funcall writer (http-status-line http))))
  (write-http-headers http writer)
  (if-let (body (http-body http))
    (progn (funcall writer body :close t)
           t)
    nil))

(defun status-code-to-text (code)
  (cond
    ((< code 200)
     (ecase code
       (100 "Continue")
       (101 "Switching Protocols")))
    ((< code 300)
     (ecase code
       (200 "OK")
       (201 "Created")
       (202 "Accepted")
       (203 "Non-Authoritative Information")
       (204 "No Content")
       (205 "Reset Content")
       (206 "Partial Content")))
    ((< code 400)
     (ecase code
       (300 "Multiple Choices")
       (301 "Moved Permanently")
       (302 "Found")
       (303 "See Other")
       (304 "Not Modified")
       (305 "Use Proxy")
       (307 "Temporary Redirect")))
    ((< code 500)
     (ecase code
       (400 "Bad Request")
       (401 "Unauthorized")
       (402 "Payment Required")
       (403 "Forbidden")
       (404 "Not Found")
       (405 "Method Not Allowed")
       (406 "Not Acceptable")
       (407 "Proxy Authentication Required")
       (408 "Request Time-out")
       (409 "Conflict")
       (410 "Gone")
       (411 "Length Required")
       (412 "Precondition Failed")
       (413 "Request Entity Too Large")
       (414 "Request-URI Too Large")
       (415 "Unsupported Media Type")
       (416 "Requested range not satisfiable")
       (417 "Expectation Failed")))
    ((<= 500 code)
     (ecase code
       (500 "Internal Server Error")
       (501 "Not Implemented")
       (502 "Bad Gateway")
       (503 "Service Unavailable")
       (504 "Gateway Time-out")
       (505 "HTTP Version not supported")))
    (T (error "Invalid status code: ~A" code))))
