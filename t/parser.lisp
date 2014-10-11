(in-package :cl-user)
(defpackage fast-http-test.parser
  (:use :cl
        :fast-http
        :fast-http.parser
        :fast-http-test.test-utils
        :prove
        :babel))
(in-package :fast-http-test.parser)

(syntax:use-syntax :interpol)

(plan nil)

;;
;; Overflow

(subtest "Header overflow error (request)"
  (let ((parser (make-ll-parser :type :request))
        (buf (bv #?"header-key: header-value\r\n")))
    (http-parse parser (make-ll-callbacks)
                (bv #?"GET / HTTP/1.1\r\n"))
    (is-error (dotimes (i 10000)
                (http-parse parser (make-ll-callbacks)
                            buf))
              'header-overflow)))

(subtest "Header overflow error (response)"
  (let ((parser (make-ll-parser :type :response))
        (buf (bv #?"header-key: header-value\r\n")))
    (http-parse parser (make-ll-callbacks)
                (bv #?"HTTP/1.0 200 OK\r\n"))
    (is-error (dotimes (i 10000)
                (http-parse parser (make-ll-callbacks) buf))
              'header-overflow)))

(subtest "No overflow for a long body"
  (flet ((run-test (type length)
           (let* ((parser (make-ll-parser :type type))
                  (header
                    (ecase type
                      (:request
                       (bv #?"POST / HTTP/1.0\r\nConnection: Keep-Alive\r\nContent-Length: ${length}\r\n\r\n"))
                      (:response
                       (bv #?"HTTP/1.0 200 OK\r\nConnection: Keep-Alive\r\nContent-Length: ${length}\r\n\r\n"))))
                  (buf (bv header :length 3000)))
             (is (http-parse parser (make-ll-callbacks)
                             buf :end (length header))
                 (length header))
             (dotimes (i length)
               (http-parse parser (make-ll-callbacks)
                           (bv "a"))))))
    (run-test :request 1000)
    (run-test :request 100000)
    (run-test :response 1000)
    (run-test :response 100000)))


(defun test-simple (&rest objects)
  (let ((parser (make-ll-parser)))
    (http-parse parser (make-ll-callbacks)
                (bv (apply #'concatenate 'string objects)))
    parser))

(defun test-parser (&rest objects)
  (let* (info
         (headers '())
         url
         (body "")
         (callbacks (make-ll-callbacks
                    :headers-complete (lambda (parser)
                                        (setf info
                                              (list :method (parser-method parser)
                                                    :status-code (if (eql (parser-status-code parser) 0)
                                                                     nil
                                                                     (parser-status-code parser))
                                                    :http-major (parser-http-major parser)
                                                    :http-minor (parser-http-minor parser)
                                                    :should-keep-alive (fast-http.parser::http-should-keep-alive-p parser))))
                    :header-field (lambda (parser data start end)
                                    (declare (ignore parser))
                                    (push (cons (babel:octets-to-string data :start start :end end)
                                                nil)
                                          headers))
                    :header-value (lambda (parser data start end)
                                    (declare (ignore parser))
                                    (setf (cdr (car headers))
                                          (append (cdr (car headers))
                                                  (list (babel:octets-to-string data :start start :end end)))))
                    :url (lambda (parser data start end)
                           (declare (ignore parser))
                           (setq url (babel:octets-to-string (subseq data start end))))
                    :body (lambda (parser data start end)
                            (declare (ignore parser))
                            (setq body
                                  (concatenate 'string
                                               body
                                               (babel:octets-to-string (subseq data start end)))))))
         (parser (make-ll-parser)))
    (http-parse parser callbacks
                (bv (apply #'concatenate 'string objects)))
    (append info
            (list :url url)
            (list :headers (loop for (field . values) in (nreverse headers)
                                 append (list field (apply #'concatenate 'string values))))
            (list :body body))))


;;
;; Requests

(is-error (test-simple #?"GET / HTP/1.1\r\n\r\n")
          'strict-error
          "Invalid version")

(ok (test-simple #?"GET / HTTP/1.1\r\n"
                 #?"Content-Type: text/plain\r\n"
                 #?"Content-Length: 6\r\n"
                 #?"\r\n"
                 "fooba")
    "Well-formed but imcomplete body")

(subtest "HTTP methods"
  (dolist (method '("DELETE"
                    "GET"
                    "HEAD"
                    "POST"
                    "PUT"
                    "OPTIONS"
                    "TRACE"
                    "COPY"
                    "LOCK"
                    "MKCOL"
                    "MOVE"
                    "PROPFIND"
                    "PROPPATCH"
                    "UNLOCK"
                    "REPORT"
                    "MKACTIVITY"
                    "CHECKOUT"
                    "MERGE"
                    "M-SEARCH"
                    "NOTIFY"
                    "SUBSCRIBE"
                    "UNSUBSCRIBE"
                    "PATCH"))
    (ok (test-simple (concatenate 'string method #?" / HTTP/1.1\r\n\r\n"))
        method)))

(subtest "HTTP bad methods"
  (dolist (method '("ASDF"
                    "C******"
                    "COLA"
                    "GEM"
                    "GETA"
                    "M****"
                    "MKCOLA"
                    "PROPPATCHA"
                    "PUN"
                    "PX"
                    "SA"
                    "hello world"
                    "0"))
    (is-error (test-simple (concatenate 'string method #?" / HTTP/1.1\r\n\r\n"))
              'invalid-method
              method)))

(is-error (test-simple #?"GET / HTTP/1.1\r\n"
                       #?"name\r\n"
                       #?" : value\r\n"
                       #?"\r\n")
          'invalid-header-token
          "illegal header field name line folding")

(ok (test-simple #?"GET / HTTP/1.1\r\n"
                 #?"X-SSL-Bullshit:   -----BEGIN CERTIFICATE-----\r\n"
                 #?"\tMIIFbTCCBFWgAwIBAgICH4cwDQYJKoZIhvcNAQEFBQAwcDELMAkGA1UEBhMCVUsx\r\n"
                 #?"\tETAPBgNVBAoTCGVTY2llbmNlMRIwEAYDVQQLEwlBdXRob3JpdHkxCzAJBgNVBAMT\r\n"
                 #?"\tAkNBMS0wKwYJKoZIhvcNAQkBFh5jYS1vcGVyYXRvckBncmlkLXN1cHBvcnQuYWMu\r\n"
                 #?"\tdWswHhcNMDYwNzI3MTQxMzI4WhcNMDcwNzI3MTQxMzI4WjBbMQswCQYDVQQGEwJV\r\n"
                 #?"\tSzERMA8GA1UEChMIZVNjaWVuY2UxEzARBgNVBAsTCk1hbmNoZXN0ZXIxCzAJBgNV\r\n"
                 #?"\tBAcTmrsogriqMWLAk1DMRcwFQYDVQQDEw5taWNoYWVsIHBhcmQYJKoZIhvcNAQEB\r\n"
                 #?"\tBQADggEPADCCAQoCggEBANPEQBgl1IaKdSS1TbhF3hEXSl72G9J+WC/1R64fAcEF\r\n"
                 #?"\tW51rEyFYiIeZGx/BVzwXbeBoNUK41OK65sxGuflMo5gLflbwJtHBRIEKAfVVp3YR\r\n"
                 #?"\tgW7cMA/s/XKgL1GEC7rQw8lIZT8RApukCGqOVHSi/F1SiFlPDxuDfmdiNzL31+sL\r\n"
                 #?"\t0iwHDdNkGjy5pyBSB8Y79dsSJtCW/iaLB0/n8Sj7HgvvZJ7x0fr+RQjYOUUfrePP\r\n"
                 #?"\tu2MSpFyf+9BbC/aXgaZuiCvSR+8Snv3xApQY+fULK/xY8h8Ua51iXoQ5jrgu2SqR\r\n"
                 #?"\twgA7BUi3G8LFzMBl8FRCDYGUDy7M6QaHXx1ZWIPWNKsCAwEAAaOCAiQwggIgMAwG\r\n"
                 #?"\tA1UdEwEB/wQCMAAwEQYJYIZIAYb4QgHTTPAQDAgWgMA4GA1UdDwEB/wQEAwID6DAs\r\n"
                 #?"\tBglghkgBhvhCAQ0EHxYdVUsgZS1TY2llbmNlIFVzZXIgQ2VydGlmaWNhdGUwHQYD\r\n"
                 #?"\tVR0OBBYEFDTt/sf9PeMaZDHkUIldrDYMNTBZMIGaBgNVHSMEgZIwgY+AFAI4qxGj\r\n"
                 #?"\tloCLDdMVKwiljjDastqooXSkcjBwMQswCQYDVQQGEwJVSzERMA8GA1UEChMIZVNj\r\n"
                 #?"\taWVuY2UxEjAQBgNVBAsTCUF1dGhvcml0eTELMAkGA1UEAxMCQ0ExLTArBgkqhkiG\r\n"
                 #?"\t9w0BCQEWHmNhLW9wZXJhdG9yQGdyaWQtc3VwcG9ydC5hYy51a4IBADApBgNVHRIE\r\n"
                 #?"\tIjAggR5jYS1vcGVyYXRvckBncmlkLXN1cHBvcnQuYWMudWswGQYDVR0gBBIwEDAO\r\n"
                 #?"\tBgwrBgEEAdkvAQEBAQYwPQYJYIZIAYb4QgEEBDAWLmh0dHA6Ly9jYS5ncmlkLXN1\r\n"
                 #?"\tcHBvcnQuYWMudmT4sopwqlBWsvcHViL2NybC9jYWNybC5jcmwwPQYJYIZIAYb4QgEDBDAWLmh0\r\n"
                 #?"\tdHA6Ly9jYS5ncmlkLXN1cHBvcnQuYWMudWsvcHViL2NybC9jYWNybC5jcmwwPwYD\r\n"
                 #?"\tVR0fBDgwNjA0oDKgMIYuaHR0cDovL2NhLmdyaWQt5hYy51ay9wdWIv\r\n"
                 #?"\tY3JsL2NhY3JsLmNybDANBgkqhkiG9w0BAQUFAAOCAQEAS/U4iiooBENGW/Hwmmd3\r\n"
                 #?"\tXCy6Zrt08YjKCzGNjorT98g8uGsqYjSxv/hmi0qlnlHs+k/3Iobc3LjS5AMYr5L8\r\n"
                 #?"\tUO7OSkgFFlLHQyC9JzPfmLCAugvzEbyv4Olnsr8hbxF1MbKZoQxUZtMVu29wjfXk\r\n"
                 #?"\thTeApBv7eaKCWpSp7MCbvgzm74izKhu3vlDk9w6qVrxePfGgpKPqfHiOoGhFnbTK\r\n"
                 #?"\twTC6o2xq5y0qZ03JonF7OJspEd3I5zKY3E+ov7/ZhW6DqT8UFvsAdjvQbXyhV8Eu\r\n"
                 #?"\tYhixw1aKEPzNjNowuIseVogKOLXxWI5vAi5HgXdS0/ES5gDGsABo4fqovUKlgop3\r\n"
                 #?"\tRA==\r\n"
                 #?"\t-----END CERTIFICATE-----\r\n"
                 #?"\r\n"))

(is (test-parser #?"GET /test HTTP/1.1\r\n"
                 #?"User-Agent: curl/7.18.0 (i486-pc-linux-gnu) libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1\r\n"
                 #?"Host: 0.0.0.0=5000\r\n"
                 #?"Accept: */*\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive T
      :url "/test"
      :headers ("User-Agent" "curl/7.18.0 (i486-pc-linux-gnu) libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1"
                "Host" "0.0.0.0=5000"
                "Accept" "*/*")
      :body "")
    "curl GET")

(is (test-parser #?"GET /favicon.ico HTTP/1.1\r\n"
                 #?"Host: 0.0.0.0=5000\r\n"
                 #?"User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0\r\n"
                 #?"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
                 #?"Accept-Language: en-us,en;q=0.5\r\n"
                 #?"Accept-Encoding: gzip,deflate\r\n"
                 #?"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n"
                 #?"Keep-Alive: 300\r\n"
                 #?"Connection: keep-alive\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/favicon.ico"
      :headers ("Host" "0.0.0.0=5000"
                "User-Agent" "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0"
                "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                "Accept-Language" "en-us,en;q=0.5"
                "Accept-Encoding" "gzip,deflate"
                "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.7"
                "Keep-Alive" "300"
                "Connection" "keep-alive")
      :body "")
    "Firefox GET")

(is (test-parser #?"GET /dumbfuck HTTP/1.1\r\n"
                 #?"aaaaaaaaaaaaa:++++++++++\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/dumbfuck"
      :headers ("aaaaaaaaaaaaa" "++++++++++")
      :body "")
    "dumbfuck")

(is (test-parser #?"GET /forums/1/topics/2375?page=1#posts-17408 HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/forums/1/topics/2375?page=1#posts-17408"
      :headers ()
      :body "")
    "fragment in URL")

(is (test-parser #?"GET /get_no_headers_no_body/world HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/get_no_headers_no_body/world"
      :headers ()
      :body "")
    "get no headers no body")

(is (test-parser #?"GET /get_one_header_no_body HTTP/1.1\r\n"
                 #?"Accept: */*\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/get_one_header_no_body"
      :headers ("Accept" "*/*")
      :body "")
    "get one header no body")

(is (test-parser #?"GET /get_funky_content_length_body_hello HTTP/1.0\r\n"
                 #?"conTENT-Length: 5\r\n"
                 #?"\r\n"
                 "HELLO")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 0
      :should-keep-alive nil
      :url "/get_funky_content_length_body_hello"
      :headers ("conTENT-Length" "5")
      :body "HELLO")
    "get funky content length body hello")

(is (test-parser #?"POST /post_identity_body_world?q=search#hey HTTP/1.1\r\n"
                 #?"Accept: */*\r\n"
                 #?"Transfer-Encoding: identity\r\n"
                 #?"Content-Length: 5\r\n"
                 #?"\r\n"
                 "World")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/post_identity_body_world?q=search#hey"
      :headers ("Accept" "*/*"
                "Transfer-Encoding" "identity"
                "Content-Length" "5")
      :body "World")
    "post identity body world")

(is (test-parser #?"POST /post_chunked_all_your_base HTTP/1.1\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"1e\r\nall your base are belong to us\r\n"
                 #?"0\r\n"
                 #?"\r\n")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/post_chunked_all_your_base"
      :headers ("Transfer-Encoding" "chunked")
      :body "all your base are belong to us")
    "post - chunked body: all your base are belong to us")

(is (test-parser #?"POST /two_chunks_mult_zero_end HTTP/1.1\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"5\r\nhello\r\n"
                 #?"6\r\n world\r\n"
                 #?"000\r\n"
                 #?"\r\n")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/two_chunks_mult_zero_end"
      :headers ("Transfer-Encoding" "chunked")
      :body "hello world")
    "two chunks ; triple zero ending")

(is (test-parser #?"POST /chunked_w_trailing_headers HTTP/1.1\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"5\r\nhello\r\n"
                 #?"6\r\n world\r\n"
                 #?"0\r\n"
                 #?"Vary: *\r\n"
                 #?"Content-Type: text/plain\r\n"
                 #?"\r\n")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/chunked_w_trailing_headers"
      :headers ("Transfer-Encoding" "chunked"
                "Vary" "*"
                "Content-Type" "text/plain")
      :body "hello world")
    "chunked with trailing headers. blech.")

(is (test-parser #?"POST /chunked_w_bullshit_after_length HTTP/1.1\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"5; ihatew3;whatthefuck=aretheseparametersfor\r\nhello\r\n"
                 #?"6; blahblah; blah\r\n world\r\n"
                 #?"0\r\n"
                 #?"\r\n")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/chunked_w_bullshit_after_length"
      :headers ("Transfer-Encoding" "chunked")
      :body "hello world")
    "with bullshit after the length")

(is (test-parser #?"GET /with_\"stupid\"_quotes?foo=\"bar\" HTTP/1.1\r\n\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/with_\"stupid\"_quotes?foo=\"bar\""
      :headers ()
      :body "")
    "with quotes")

(is (test-parser #?"GET /test HTTP/1.0\r\n"
                 #?"Host: 0.0.0.0:5000\r\n"
                 #?"User-Agent: ApacheBench/2.3\r\n"
                 #?"Accept: */*\r\n\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 0
      :should-keep-alive nil
      :url "/test"
      :headers ("Host" "0.0.0.0:5000"
                "User-Agent" "ApacheBench/2.3"
                "Accept" "*/*")
      :body "")
    "ApacheBench GET")

(is (test-parser #?"GET /test.cgi?foo=bar?baz HTTP/1.1\r\n\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/test.cgi?foo=bar?baz"
      :headers ()
      :body "")
    "Query URL with question mark")

(is (test-parser #?"\r\nGET /test HTTP/1.1\r\n\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/test"
      :headers ()
      :body "")
    "Newline prefix GET")

(is (test-parser #?"GET /demo HTTP/1.1\r\n"
                 #?"Host: example.com\r\n"
                 #?"Connection: Upgrade\r\n"
                 #?"Sec-WebSocket-Key2: 12998 5 Y3 1  .P00\r\n"
                 #?"Sec-WebSocket-Protocol: sample\r\n"
                 #?"Upgrade: WebSocket\r\n"
                 #?"Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5\r\n"
                 #?"Origin: http://example.com\r\n"
                 #?"\r\n"
                 "Hot diggity dogg")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/demo"
      :headers ("Host" "example.com"
                "Connection" "Upgrade"
                "Sec-WebSocket-Key2" "12998 5 Y3 1  .P00"
                "Sec-WebSocket-Protocol" "sample"
                "Upgrade" "WebSocket"
                "Sec-WebSocket-Key1" "4 @1  46546xW%0l 1 5"
                "Origin" "http://example.com")
      :body "")
    "Upgrade request")

(is (test-parser #?"CONNECT 0-home0.netscape.com:443 HTTP/1.0\r\n"
                 #?"User-agent: Mozilla/1.1N\r\n"
                 #?"Proxy-authorization: basic aGVsbG86d29ybGQ=\r\n"
                 #?"\r\n"
                 #?"some data\r\n"
                 "and yet even more data")
    '(:method :connect
      :status-code nil
      :http-major 1
      :http-minor 0
      :should-keep-alive nil
      :url "0-home0.netscape.com:443"
      :headers ("User-agent" "Mozilla/1.1N"
                "Proxy-authorization" "basic aGVsbG86d29ybGQ=")
      :body "")
    "CONNECT request")

(is (test-parser #?"REPORT /test HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :report
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/test"
      :headers ()
      :body "")
    "REPORT request")

(is (test-parser #?"GET /\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 0
      :http-minor 9
      :should-keep-alive nil
      :url "/"
      :headers ()
      :body "")
    "request with no HTTP version")

(is (test-parser #?"M-SEARCH * HTTP/1.1\r\n"
                 #?"HOST: 239.255.255.250:1900\r\n"
                 #?"MAN: \"ssdp:discover\"\r\n"
                 #?"ST: \"ssdp:all\"\r\n"
                 #?"\r\n")
    '(:method :m-search
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "*"
      :headers ("HOST" "239.255.255.250:1900"
                "MAN" "\"ssdp:discover\""
                "ST" "\"ssdp:all\"")
      :body "")
    "M-SEARCH request")

(is (test-parser #?"GET / HTTP/1.1\r\n"
                 #?"Line1:   abc\r\n"
                 #?"\tdef\r\n"
                 #?" ghi\r\n"
                 #?"\t\tjkl\r\n"
                 #?"  mno \r\n"
                 #?"\t \tqrs\r\n"
                 #?"Line2: \t line2\t\r\n"
                 #?"Line3:\r\n"
                 #?" line3\r\n"
                 #?"Line4: \r\n"
                 #?" \r\n"
                 #?"Connection:\r\n"
                 #?" close\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url "/"
      :headers ("Line1" #?"abc\tdef ghi\t\tjkl  mno \t \tqrs"
                "Line2" #?"line2\t"
                "Line3" "line3"
                "Line4" ""
                "Connection" "close")
      :body "")
    "line folding in header value")

(is (test-parser #?"GET http://hypnotoad.org?hail=all HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "http://hypnotoad.org?hail=all"
      :headers ()
      :body "")
    "host terminated by a query string")

(is (test-parser #?"GET http://hypnotoad.org:1234?hail=all HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "http://hypnotoad.org:1234?hail=all"
      :headers ()
      :body "")
    "host:port terminated by a query string")

(is (test-parser #?"GET http://hypnotoad.org:1234 HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "http://hypnotoad.org:1234"
      :headers ()
      :body "")
    "host:port terminated by a space")

(is (test-parser #?"PATCH /file.txt HTTP/1.1\r\n"
                 #?"Host: www.example.com\r\n"
                 #?"Content-Type: application/example\r\n"
                 #?"If-Match: \"e0023aa4e\"\r\n"
                 #?"Content-Length: 10\r\n"
                 #?"\r\n"
                 "cccccccccc")
    '(:method :patch
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/file.txt"
      :headers ("Host" "www.example.com"
                "Content-Type" "application/example"
                "If-Match" "\"e0023aa4e\""
                "Content-Length" "10")
      :body "cccccccccc")
    "PATCH request")

(is (test-parser #?"CONNECT HOME0.NETSCAPE.COM:443 HTTP/1.0\r\n"
                 #?"User-agent: Mozilla/1.1N\r\n"
                 #?"Proxy-authorization: basic aGVsbG86d29ybGQ=\r\n"
                 #?"\r\n")
    '(:method :connect
      :status-code nil
      :http-major 1
      :http-minor 0
      :should-keep-alive nil
      :url "HOME0.NETSCAPE.COM:443"
      :headers ("User-agent" "Mozilla/1.1N"
                "Proxy-authorization" "basic aGVsbG86d29ybGQ=")
      :body "")
    "CONNECT caps request")

(is (test-parser #?"GET /δ¶/δt/pope?q=1#narf HTTP/1.1\r\n"
                 #?"Host: github.com\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/δ¶/δt/pope?q=1#narf"
      :headers ("Host" "github.com")
      :body "")
    "utf-8 path request")

(is (test-parser #?"CONNECT home_0.netscape.com:443 HTTP/1.0\r\n"
                 #?"User-agent: Mozilla/1.1N\r\n"
                 #?"Proxy-authorization: basic aGVsbG86d29ybGQ=\r\n"
                 #?"\r\n")
    '(:method :connect
      :status-code nil
      :http-major 1
      :http-minor 0
      :should-keep-alive nil
      :url "home_0.netscape.com:443"
      :headers ("User-agent" "Mozilla/1.1N"
                "Proxy-authorization" "basic aGVsbG86d29ybGQ=")
      :body "")
    "underscore in hostname")

(is (test-parser #?"POST / HTTP/1.1\r\n"
                 #?"Host: www.example.com\r\n"
                 #?"Content-Type: application/x-www-form-urlencoded\r\n"
                 #?"Content-Length: 4\r\n"
                 #?"\r\n"
                 #?"q=42\r\n")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/"
      :headers ("Host" "www.example.com"
                "Content-Type" "application/x-www-form-urlencoded"
                "Content-Length" "4")
      :body "q=42")
    "eat CRLF between requests, no \"Connection: close\" header")

(is (test-parser #?"POST / HTTP/1.1\r\n"
                 #?"Host: www.example.com\r\n"
                 #?"Content-Type: application/x-www-form-urlencoded\r\n"
                 #?"Content-Length: 4\r\n"
                 #?"Connection: close\r\n"
                 #?"\r\n"
                 #?"q=42\r\n")
    '(:method :post
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url "/"
      :headers ("Host" "www.example.com"
                "Content-Type" "application/x-www-form-urlencoded"
                "Content-Length" "4"
                "Connection" "close")
      :body "q=42")
    "eat CRLF between requests even if \"Connection: close\" is set")

(is (test-parser #?"PURGE /file.txt HTTP/1.1\r\n"
                 #?"Host: www.example.com\r\n"
                 #?"\r\n")
    '(:method :purge
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/file.txt"
      :headers ("Host" "www.example.com")
      :body "")
    "PURGE request")

(is (test-parser #?"SEARCH / HTTP/1.1\r\n"
                 #?"Host: www.example.com\r\n"
                 #?"\r\n")
    '(:method :search
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "/"
      :headers ("Host" "www.example.com")
      :body "")
    "SEARCH request")

(is (test-parser #?"GET http://a%12:b!&*$@hypnotoad.org:1234/toto HTTP/1.1\r\n"
                 #?"\r\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url "http://a%12:b!&*$@hypnotoad.org:1234/toto"
      :headers ()
      :body "")
    "host:port and basic_auth")

(is (test-parser #?"GET / HTTP/1.1\n"
                 #?"Line1:   abc\n"
                 #?"\tdef\n"
                 #?" ghi\n"
                 #?"\t\tjkl\n"
                 #?"  mno \n"
                 #?"\t \tqrs\n"
                 #?"Line2: \t line2\t\n"
                 #?"Line3:\n"
                 #?" line3\n"
                 #?"Line4: \n"
                 #?" \n"
                 #?"Connection:\n"
                 #?" close\n"
                 #?"\n")
    '(:method :get
      :status-code nil
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url "/"
      :headers ("Line1" #?"abc\tdef ghi\t\tjkl  mno \t \tqrs"
                "Line2" #?"line2\t"
                "Line3" "line3"
                "Line4" ""
                "Connection" "close")
      :body "")
    "line folding in header value")


;;
;; Responses

(is (test-parser #?"HTTP/1.1 301 Moved Permanently\r\n"
                 #?"Location: http://www.google.com/\r\n"
                 #?"Content-Type: text/html; charset=UTF-8\r\n"
                 #?"Date: Sun, 26 Apr 2009 11:11:49 GMT\r\n"
                 #?"Expires: Tue, 26 May 2009 11:11:49 GMT\r\n"
                 #?"X-$PrototypeBI-Version: 1.6.0.3\r\n"
                 #?"Cache-Control: public, max-age=2592000\r\n"
                 #?"Server: gws\r\n"
                 #?"Content-Length:  219  \r\n"
                 #?"\r\n"
                 #?"<HTML><HEAD><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">\n"
                 #?"<TITLE>301 Moved</TITLE></HEAD><BODY>\n"
                 #?"<H1>301 Moved</H1>\n"
                 #?"The document has moved\n"
                 #?"<A HREF=\"http://www.google.com/\">here</A>.\r\n"
                 #?"</BODY></HTML>\r\n")
    `(:method nil
      :status-code 301
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url nil
      :headers ("Location" "http://www.google.com/"
                "Content-Type" "text/html; charset=UTF-8"
                "Date" "Sun, 26 Apr 2009 11:11:49 GMT"
                "Expires" "Tue, 26 May 2009 11:11:49 GMT"
                "X-$PrototypeBI-Version" "1.6.0.3"
                "Cache-Control" "public, max-age=2592000"
                "Server" "gws"
                "Content-Length" "219  ")
      :body ,(concatenate 'string
              #?"<HTML><HEAD><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">\n"
              #?"<TITLE>301 Moved</TITLE></HEAD><BODY>\n"
              #?"<H1>301 Moved</H1>\n"
              #?"The document has moved\n"
              #?"<A HREF=\"http://www.google.com/\">here</A>.\r\n"
              #?"</BODY></HTML>\r\n"))
    "Google 301")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Date: Tue, 04 Aug 2009 07:59:32 GMT\r\n"
                 #?"Server: Apache\r\n"
                 #?"X-Powered-By: Servlet/2.5 JSP/2.1\r\n"
                 #?"Content-Type: text/xml; charset=utf-8\r\n"
                 #?"Connection: close\r\n"
                 #?"\r\n"
                 #?"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                 #?"<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">\n"
                 #?"  <SOAP-ENV:Body>\n"
                 #?"    <SOAP-ENV:Fault>\n"
                 #?"       <faultcode>SOAP-ENV:Client</faultcode>\n"
                 #?"       <faultstring>Client Error</faultstring>\n"
                 #?"    </SOAP-ENV:Fault>\n"
                 #?"  </SOAP-ENV:Body>\n"
                 #?"</SOAP-ENV:Envelope>")
    `(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Date" "Tue, 04 Aug 2009 07:59:32 GMT"
                "Server" "Apache"
                "X-Powered-By" "Servlet/2.5 JSP/2.1"
                "Content-Type" "text/xml; charset=utf-8"
                "Connection" "close")
      :body ,(concatenate 'string
              #?"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              #?"<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">\n"
              #?"  <SOAP-ENV:Body>\n"
              #?"    <SOAP-ENV:Fault>\n"
              #?"       <faultcode>SOAP-ENV:Client</faultcode>\n"
              #?"       <faultstring>Client Error</faultstring>\n"
              #?"    </SOAP-ENV:Fault>\n"
              #?"  </SOAP-ENV:Body>\n"
              #?"</SOAP-ENV:Envelope>"))
    "no Content-Length response")

(is (test-parser #?"HTTP/1.1 404 Not Found\r\n\r\n")
    '(:method nil
      :status-code 404
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ()
      :body "")
    "404 no headers and no body")

(is (test-parser #?"HTTP/1.1 301\r\n\r\n")
    '(:method nil
      :status-code 301
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ()
      :body "")
    "301 no response phase")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Content-Type: text/plain\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"25  \r\n"
                 #?"This is the data in the first chunk\r\n"
                 #?"\r\n"
                 #?"1C\r\n"
                 #?"and this is the second one\r\n"
                 #?"\r\n"
                 #?"0  \r\n"
                 #?"\r\n")
    `(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url nil
      :headers ("Content-Type" "text/plain"
                "Transfer-Encoding" "chunked")
       :body ,(concatenate 'string
               #?"This is the data in the first chunk\r\n"
               #?"and this is the second one\r\n"))
    "200 trailing space on chunked body")

(is (test-parser #?"HTTP/1.1 200 OK\n"
                 #?"Content-Type: text/html; charset=utf-8\n"
                 #?"Connection: close\n"
                 #?"\n"
                 #?"these headers are from http://news.ycombinator.com/")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Content-Type" "text/html; charset=utf-8"
                "Connection" "close")
      :body "these headers are from http://news.ycombinator.com/")
    "no carriage ret")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Content-Type: text/html; charset=UTF-8\r\n"
                 #?"Content-Length: 11\r\n"
                 #?"Proxy-Connection: close\r\n"
                 #?"Date: Thu, 31 Dec 2009 20:55:48 +0000\r\n"
                 #?"\r\n"
                 "hello world")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Content-Type" "text/html; charset=UTF-8"
                "Content-Length" "11"
                "Proxy-Connection" "close"
                "Date" "Thu, 31 Dec 2009 20:55:48 +0000")
      :body "hello world")
    "proxy connection")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Server: DCLK-AdSvr\r\n"
                 #?"Content-Type: text/xml\r\n"
                 #?"Content-Length: 0\r\n"
                 #?"DCLK_imp: v7;x;114750856;0-0;0;17820020;0/0;21603567/21621457/1;;~~okv=;dcmt=text/xml;;~~cs=o\r\n\r\n")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url nil
      :headers ("Server" "DCLK-AdSvr"
                "Content-Type" "text/xml"
                "Content-Length" "0"
                "DCLK_imp" "v7;x;114750856;0-0;0;17820020;0/0;21603567/21621457/1;;~~okv=;dcmt=text/xml;;~~cs=o")
      :body "")
    "underscore header key")

(is (test-parser #?"HTTP/1.0 301 Moved Permanently\r\n"
                 #?"Date: Thu, 03 Jun 2010 09:56:32 GMT\r\n"
                 #?"Server: Apache/2.2.3 (Red Hat)\r\n"
                 #?"Cache-Control: public\r\n"
                 #?"Pragma: \r\n"
                 #?"Location: http://www.bonjourmadame.fr/\r\n"
                 #?"Vary: Accept-Encoding\r\n"
                 #?"Content-Length: 0\r\n"
                 #?"Content-Type: text/html; charset=UTF-8\r\n"
                 #?"Connection: keep-alive\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 301
      :http-major 1
      :http-minor 0
      :should-keep-alive t
      :url nil
      :headers ("Date" "Thu, 03 Jun 2010 09:56:32 GMT"
                "Server" "Apache/2.2.3 (Red Hat)"
                "Cache-Control" "public"
                "Pragma" ""
                "Location" "http://www.bonjourmadame.fr/"
                "Vary" "Accept-Encoding"
                "Content-Length" "0"
                "Content-Type" "text/html; charset=UTF-8"
                "Connection" "keep-alive")
      :body "")
    "bonjourmadame.fr")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Date: Tue, 28 Sep 2010 01:14:13 GMT\r\n"
                 #?"Server: Apache\r\n"
                 #?"Cache-Control: no-cache, must-revalidate\r\n"
                 #?"Expires: Mon, 26 Jul 1997 05:00:00 GMT\r\n"
                 #?".et-Cookie: PlaxoCS=1274804622353690521; path=/; domain=.plaxo.com\r\n"
                 #?"Vary: Accept-Encoding\r\n"
                 #?"_eep-Alive: timeout=45\r\n"
                 #?"_onnection: Keep-Alive\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"Content-Type: text/html\r\n"
                 #?"Connection: close\r\n"
                 #?"\r\n"
                 #?"0\r\n\r\n")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Date" "Tue, 28 Sep 2010 01:14:13 GMT"
                "Server" "Apache"
                "Cache-Control" "no-cache, must-revalidate"
                "Expires" "Mon, 26 Jul 1997 05:00:00 GMT"
                ".et-Cookie" "PlaxoCS=1274804622353690521; path=/; domain=.plaxo.com"
                "Vary" "Accept-Encoding"
                "_eep-Alive" "timeout=45"
                "_onnection" "Keep-Alive"
                "Transfer-Encoding" "chunked"
                "Content-Type" "text/html"
                "Connection" "close")
      :body "")
    "field underscore")

(is (test-parser #?"HTTP/1.1 500 Oriëntatieprobleem\r\n"
                 #?"Date: Fri, 5 Nov 2010 23:07:12 GMT+2\r\n"
                 #?"Content-Length: 0\r\n"
                 #?"Connection: close\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 500
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Date" "Fri, 5 Nov 2010 23:07:12 GMT+2"
                "Content-Length" "0"
                "Connection" "close")
      :body "")
    "non-ASCII in status line")

(is (test-parser #?"HTTP/0.9 200 OK\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 200
      :http-major 0
      :http-minor 9
      :should-keep-alive nil
      :url nil
      :headers ()
      :body "")
    "HTTP version 0.9")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Content-Type: text/plain\r\n"
                 #?"\r\n"
                 "hello world")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Content-Type" "text/plain")
      :body "hello world")
    "neither Content-Length nor Transfer-Encoding response")

(is (test-parser #?"HTTP/1.0 200 OK\r\n"
                 #?"Connection: keep-alive\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 0
      :should-keep-alive nil
      :url nil
      :headers ("Connection" "keep-alive")
      :body "")
    "HTTP/1.0 with keep-alive and EOF-terminated 200 status")

(is (test-parser #?"HTTP/1.0 204 No content\r\n"
                 #?"Connection: keep-alive\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 204
      :http-major 1
      :http-minor 0
      :should-keep-alive t
      :url nil
      :headers ("Connection" "keep-alive")
      :body "")
    "HTTP/1.0 with keep-alive and a 204 status")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ()
      :body "")
    "HTTP/1.1 with an EOF-terminated 200 status")

(is (test-parser #?"HTTP/1.1 204 No content\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 204
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url nil
      :headers ()
      :body "")
    "HTTP/1.1 with a 204 status")

(is (test-parser #?"HTTP/1.1 204 No content\r\n"
                 #?"Connection: close\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 204
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ("Connection" "close")
      :body "")
    "HTTP/1.1 with a 204 status and keep-alive disabled")

(is (test-parser #?"HTTP/1.1 200 OK\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"0\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url nil
      :headers ("Transfer-Encoding" "chunked")
      :body "")
    "HTTP/1.1 with chunked endocing and a 200 response")

(is (test-parser #?"HTTP/1.1 301 MovedPermanently\r\n"
                 #?"Date: Wed, 15 May 2013 17:06:33 GMT\r\n"
                 #?"Server: Server\r\n"
                 #?"x-amz-id-1: 0GPHKXSJQ826RK7GZEB2\r\n"
                 #?"p3p: policyref=\"http://www.amazon.com/w3c/p3p.xml\",CP=\"CAO DSP LAW CUR ADM IVAo IVDo CONo OTPo OUR DELi PUBi OTRi BUS PHY ONL UNI PUR FIN COM NAV INT DEM CNT STA HEA PRE LOC GOV OTC \"\r\n"
                 #?"x-amz-id-2: STN69VZxIFSz9YJLbz1GDbxpbjG6Qjmmq5E3DxRhOUw+Et0p4hr7c/Q8qNcx4oAD\r\n"
                 #?"Location: http://www.amazon.com/Dan-Brown/e/B000AP9DSU/ref=s9_pop_gw_al1?_encoding=UTF8&refinementId=618073011&pf_rd_m=ATVPDKIKX0DER&pf_rd_s=center-2&pf_rd_r=0SHYY5BZXN3KR20BNFAY&pf_rd_t=101&pf_rd_p=1263340922&pf_rd_i=507846\r\n"
                 #?"Vary: Accept-Encoding,User-Agent\r\n"
                 #?"Content-Type: text/html; charset=ISO-8859-1\r\n"
                 #?"Transfer-Encoding: chunked\r\n"
                 #?"\r\n"
                 #?"1\r\n"
                 #?"\n\r\n"
                 #?"0\r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 301
      :http-major 1
      :http-minor 1
      :should-keep-alive t
      :url nil
      :headers ("Date" "Wed, 15 May 2013 17:06:33 GMT"
                "Server" "Server"
                "x-amz-id-1" "0GPHKXSJQ826RK7GZEB2"
                "p3p" "policyref=\"http://www.amazon.com/w3c/p3p.xml\",CP=\"CAO DSP LAW CUR ADM IVAo IVDo CONo OTPo OUR DELi PUBi OTRi BUS PHY ONL UNI PUR FIN COM NAV INT DEM CNT STA HEA PRE LOC GOV OTC \""
                "x-amz-id-2" "STN69VZxIFSz9YJLbz1GDbxpbjG6Qjmmq5E3DxRhOUw+Et0p4hr7c/Q8qNcx4oAD"
                "Location" "http://www.amazon.com/Dan-Brown/e/B000AP9DSU/ref=s9_pop_gw_al1?_encoding=UTF8&refinementId=618073011&pf_rd_m=ATVPDKIKX0DER&pf_rd_s=center-2&pf_rd_r=0SHYY5BZXN3KR20BNFAY&pf_rd_t=101&pf_rd_p=1263340922&pf_rd_i=507846"
                "Vary" "Accept-Encoding,User-Agent"
                "Content-Type" "text/html; charset=ISO-8859-1"
                "Transfer-Encoding" "chunked")
      :body #?"\n")
    "amazon.com")

(is (test-parser #?"HTTP/1.1 200 \r\n"
                 #?"\r\n")
    '(:method nil
      :status-code 200
      :http-major 1
      :http-minor 1
      :should-keep-alive nil
      :url nil
      :headers ()
      :body "")
    "empty reason phrase after space")


;;
;; parse-header-value-parameters

(defun test-parse-header-parameters (data expected &optional description)
  (let (header-value
        parameters)
    (parse-header-value-parameters data
                                   :header-value-callback
                                   (lambda (data start end)
                                     (setq header-value (subseq data start end)))
                                   :header-parameter-key-callback
                                   (lambda (data start end)
                                     (push (subseq data start end)
                                           parameters))
                                   :header-parameter-value-callback
                                   (lambda (data start end)
                                     (push (subseq data start end)
                                           parameters)))
    (is (list header-value (nreverse parameters))
        expected
        description)))

(test-parse-header-parameters "none"
                              '("none" ())
                              "no parameters")

(test-parse-header-parameters "none;"
                              '("none" ())
                              "no parameters")

(test-parse-header-parameters "form-data; name=\"key\""
                              '("form-data" ("name" "key"))
                              "quoted-string value")

(test-parse-header-parameters "form-data; name=key"
                              '("form-data" ("name" "key"))
                              "tokens value")

(test-parse-header-parameters "form-data; name=key;"
                              '("form-data" ("name" "key"))
                              "ends with a needless semi-colon")

(is-error (test-parse-header-parameters "form-data; name=\"key" nil)
          'invalid-eof-state
          "Unexpected EOF when parsing a quoted-string")

(test-parse-header-parameters #?"form-data; name=\"key\nmultiline\""
                              '("form-data" ("name" #?"key\nmultiline"))
                              "multiline")

(test-parse-header-parameters #?"form-data; name=\"フィールド1\""
                              '("form-data" ("name" #?"フィールド1"))
                              "utf-8")

(test-parse-header-parameters #?"form-data; name=\"upload1\"; filename=\"file.txt\""
                              '("form-data" ("name" "upload1"
                                             "filename" "file.txt"))
                              "multiple parameters")

(test-parse-header-parameters #?"gzip;q=1.0, identity; q=0.5, *;q=0"
                              '("gzip" ("q" "1.0, identity"
                                        "q" "0.5, *"
                                        "q" "0"))
                              "Accept-Encoding")

(finalize)
