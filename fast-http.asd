#|
  This file is a part of fast-http project.
  URL: http://github.com/fukamachi/fast-http
  Copyright (c) 2014 Eitaro Fukamachi <e.arrows@gmail.com>
|#

(in-package :cl-user)
(defpackage fast-http-asd
  (:use :cl :asdf))
(in-package :fast-http-asd)

(defsystem fast-http
  :name "Fast HTTP Parser"
  :description "A fast HTTP protocol parser in Common Lisp"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:alexandria
               :cl-utilities
               :babel
               :xsubseq
               :log4cl

               ;; for body-buffer
               :cl-fad
               :flexi-streams)
  :components ((:module "src"
                :components
                ((:file "fast-http" :depends-on ("http" "parser" "unparser" "multipart-parser" "body-buffer" "byte-vector" "error"))
                 (:file "http")
                 (:file "parser"  :depends-on ("byte-vector" "variables" "error" "util"))
                 (:file "unparser" :depends-on ("http"))
                 (:file "multipart-parser" :depends-on ("parser" "byte-vector" "error"))
                 (:file "byte-vector")
                 (:file "body-buffer" :depends-on ("error"))
                 (:file "stateless-parser" :depends-on ("error" "byte-vector" "util"))
                 (:file "variables")
                 (:file "error")
                 (:file "util" :depends-on ("error")))))
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op fast-http-test))))
