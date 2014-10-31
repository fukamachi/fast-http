(in-package :cl-user)
(defpackage fast-http.variables
  (:use :cl)
  (:import-from :alexandria
                :define-constant)
  (:export
   :+state-map+

   :+state-dead+

   :+state-start-req-or-res+
   :+state-res-or-resp-H+
   :+state-start-res+
   :+state-res-H+
   :+state-res-HT+
   :+state-res-HTT+
   :+state-res-HTTP+
   :+state-res-first-http-major+
   :+state-res-http-major+
   :+state-res-first-http-minor+
   :+state-res-http-minor+
   :+state-res-first-status-code+
   :+state-res-status-code+
   :+state-res-status-start+
   :+state-res-status+
   :+state-res-line-almost-done+

   :+state-start-req+

   :+state-req-method+
   :+state-req-spaces-before-url+
   :+state-req-schema+
   :+state-req-schema-slash+
   :+state-req-schema-slash-slash+
   :+state-req-server-start+
   :+state-req-server+
   :+state-req-server-with-at+
   :+state-req-path+
   :+state-req-query-string-start+
   :+state-req-query-string+
   :+state-req-fragment-start+
   :+state-req-fragment+
   :+state-req-http-start+
   :+state-req-http-H+
   :+state-req-http-HT+
   :+state-req-http-HTT+
   :+state-req-http-HTTP+
   :+state-req-first-http-major+
   :+state-req-http-major+
   :+state-req-first-http-minor+
   :+state-req-http-minor+
   :+state-req-line-almost-done+

   :+state-header-field-start+
   :+state-header-field+
   :+state-header-value-discard-ws+
   :+state-header-value-discard-ws-almost-done+
   :+state-header-value-discard-lws+
   :+state-header-value-start+
   :+state-header-value+
   :+state-header-value-lws+

   :+state-header-almost-done+

   :+state-chunk-size-start+
   :+state-chunk-size+
   :+state-chunk-parameters+
   :+state-chunk-size-almost-done+

   :+state-headers-almost-done+
   :+state-headers-done+

   :+state-chunk-data+
   :+state-chunk-data-almost-done+
   :+state-chunk-data-done+

   :+state-body-identity+
   :+state-body-identity-eof+

   :+state-message-done+

   :+content-length+
   :+transfer-encoding+
   :+upgrade+
   :+chunked+

   :+header-state-general+
   :+header-state-c+
   :+header-state-co+
   :+header-state-con+

   :+header-state-matching-connection+
   :+header-state-matching-proxy-connection+
   :+header-state-matching-content-length+
   :+header-state-matching-transfer-encoding+
   :+header-state-matching-upgrade+

   :+header-state-connection+
   :+header-state-content-length+
   :+header-state-transfer-encoding+
   :+header-state-upgrade+

   :+header-state-matching-transfer-encoding-chunked+
   :+header-state-matching-connection-keep-alive+
   :+header-state-matching-connection-close+

   :+header-state-transfer-encoding-chunked+
   :+header-state-connection-keep-alive+
   :+header-state-connection-close+))
(in-package :fast-http.variables)

;;
;; States

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +state-map+
      (make-array 62
                  :element-type 'symbol
                  :initial-contents
                  '(dead

                    start-req-or-res
                    res-or-resp-H
                    start-res
                    res-H
                    res-HT
                    res-HTT
                    res-HTTP
                    res-first-http-major
                    res-http-major
                    res-first-http-minor
                    res-http-minor
                    res-first-status-code
                    res-status-code
                    res-status-start
                    res-status
                    res-line-almost-done

                    start-req

                    req-method
                    req-spaces-before-url
                    req-schema
                    req-schema-slash
                    req-schema-slash-slash
                    req-server-start
                    req-server
                    req-server-with-at
                    req-path
                    req-query-string-start
                    req-query-string
                    req-fragment-start
                    req-fragment
                    req-http-start
                    req-http-H
                    req-http-HT
                    req-http-HTT
                    req-http-HTTP
                    req-first-http-major
                    req-http-major
                    req-first-http-minor
                    req-http-minor
                    req-line-almost-done

                    header-field-start
                    header-field
                    header-value-discard-ws
                    header-value-discard-ws-almost-done
                    header-value-discard-lws
                    header-value-start
                    header-value
                    header-value-lws

                    header-almost-done

                    chunk-size-start
                    chunk-size
                    chunk-parameters
                    chunk-size-almost-done

                    headers-almost-done
                    headers-done

                    chunk-data
                    chunk-data-almost-done
                    chunk-data-done

                    body-identity
                    body-identity-eof

                    message-done))
    :test 'equalp))

#.`(progn
     ,@(loop for i from 0
             for state across +state-map+
             for state-name = (intern (format nil "+STATE-~A+" state))
             collect `(declaim (type fixnum ,state-name))
             collect `(defconstant ,state-name ,i)))


;;
;; Header States

(define-constant +content-length+ "content-length" :test 'equalp)
(define-constant +transfer-encoding+ "transfer-encoding" :test 'equalp)
(define-constant +upgrade+ "upgrade" :test 'equalp)
(define-constant +chunked+ "chunked" :test 'equalp)

#.`(progn
     ,@(loop for i from 0
             for state in '(general

                            matching-content-length
                            matching-transfer-encoding
                            matching-upgrade

                            content-length
                            transfer-encoding
                            upgrade

                            matching-transfer-encoding-chunked

                            transfer-encoding-chunked)
             for state-name = (intern (format nil "+HEADER-STATE-~A+" state))
             collect `(declaim (type fixnum ,state-name))
             collect `(defconstant ,state-name ,i)))
