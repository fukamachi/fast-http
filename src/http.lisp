(in-package :cl-user)
(defpackage fast-http.http
  (:use :cl)
  (:export :http
           :http-request
           :http-response

           :make-http-request
           :make-http-response

           :http-p
           :http-request-p
           :http-response-p

           :http-version
           :http-headers
           :http-body
           :http-method
           :http-resource
           :http-status
           :http-status-text))
(in-package :fast-http.http)

(defstruct http
  "Base structure class extended by HTTP-REQUEST and HTTP-RESPONSE."
  version
  headers
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
