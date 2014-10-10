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
               :log4cl)
  :components ((:module "src"
                :components
                ((:file "fast-http" :depends-on ("parser" "byte-vector" "error" "subseqs"))
                 (:file "parser"  :depends-on ("byte-vector" "url" "variables" "error" "util"))
                 (:file "byte-vector")
                 (:file "subseqs" :depends-on ("byte-vector"))
                 (:file "url" :depends-on ("variables" "util"))
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
