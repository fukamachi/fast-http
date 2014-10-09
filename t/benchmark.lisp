(in-package :cl-user)
(defpackage fast-http-test.bench
  (:use :cl
        :fast-http)
  (:export :run-benchmark
           :run-profile))
(in-package :fast-http-test.bench)

(syntax:use-syntax :interpol)

(defun run-benchmark ()
  (let ((parser (make-parser))
        (callbacks (make-parser-callbacks))
        (data (babel:string-to-octets #?"GET /cookies HTTP/1.1\r\nHost: 127.0.0.1:8090\r\nConnection: keep-alive\r\nCache-Control: max-age=0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\nCookie: name=wookie\r\n\r\n")))
    (time
     (loop repeat 100000 do
       (http-parse parser callbacks data)))))

#+sbcl
(defun run-profile ()
  (sb-profile:profile "FAST-HTTP" "FAST-HTTP.ERROR" "FAST-HTTP.BYTE-VECTOR" "FAST-HTTP.UTIL")
  (run-benchmark)
  (sb-profile:report)
  (sb-profile:unprofile "FAST-HTTP" "FAST-HTTP.ERROR" "FAST-HTTP.BYTE-VECTOR" "FAST-HTTP.UTIL"))
