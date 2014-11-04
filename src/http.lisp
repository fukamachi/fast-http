(in-package :cl-user)
(defpackage fast-http.http
  (:use :cl)
  (:export :http
           :make-http
           :http-p

           :http-major-version
           :http-minor-version
           :http-method
           :http-status-code
           :http-content-length
           :http-chunked-p
           :http-upgrade-p

           :http-header-read
           :http-mark
           :http-state

           ;; Types
           :status-code

           ;; States
           :+state-first-line+
           :+state-headers+
           :+state-chunk-size+
           :+state-body+))
(in-package :fast-http.http)

;;
;; Types

(deftype status-code () '(integer 0 10000))

;;
;; States

(defconstant +state-first-line+ 0)
(defconstant +state-headers+ 1)
(defconstant +state-chunk-size+ 2)
(defconstant +state-body+ 3)

(defstruct (http (:conc-name :http-))
  (method nil :type symbol)
  (major-version 0 :type fixnum)
  (minor-version 9 :type fixnum)
  (status-code 0 :type status-code)
  (content-length nil :type (or null integer))
  (chunked-p nil :type boolean)
  (upgrade-p nil :type boolean)

  ;; private
  (header-read 0 :type fixnum)
  (mark -1 :type fixnum)
  (state +state-first-line+ :type fixnum))
