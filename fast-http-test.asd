#|
  This file is a part of fast-http project.
  URL: http://github.com/fukamachi/fast-http
  Copyright (c) 2014 Eitaro Fukamachi <e.arrows@gmail.com>
|#

(in-package :cl-user)
(defpackage fast-http-test-asd
  (:use :cl :asdf))
(in-package :fast-http-test-asd)

(defsystem fast-http-test
  :depends-on (:fast-http
               :babel
               :cl-syntax-interpol
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "fast-http")
                 (:file "benchmark"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
