(in-package :cl-user)
(defpackage fast-http.parser
  (:use :cl
        :fast-http.byte-vector
        :fast-http.error
        :fast-http.variables
        :fast-http.util)
  (:import-from :alexandria
                :define-constant
                :when-let
                :with-gensyms)
  (:export :http-parse
           :http-parse-headers
           :parse-header-value-parameters
           :ll-parser
           :ll-callbacks
           :make-ll-parser
           :make-ll-callbacks
           :parser-method
           :parser-status-code
           :parser-chunked-p
           :parser-upgrade-p
           :parser-content-length
           :parser-http-major
           :parser-http-minor
           :+max-content-length+))
(in-package :fast-http.parser)

(declaim (type fixnum +http-max-header-size+))
(defconstant +http-max-header-size+
  (* 80 1024))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type (unsigned-byte 64) +max-content-length+))
  (defconstant +max-content-length+
    (1- (expt 2 64))))

(deftype pointer () '(unsigned-byte 64))
(deftype http-version () '(integer 0 1000))
(deftype status-code () '(integer 0 1000))

(defconstant +ok+ -1)


;;
;; Data types

(defstruct (ll-parser (:constructor make-ll-parser (&key
                                                      (type :both)
                                                    &aux
                                                      (state (case type
                                                               (:request +state-start-req+)
                                                               (:response +state-start-res+)
                                                               (otherwise +state-start-req-or-res+)))))
                      (:conc-name :parser-))
  (type :both :type keyword)
  (flags 6 :type fixnum)
  (state -1 :type fixnum)
  (header-state -1 :type fixnum)
  (index 0 :type fixnum)

  (header-read 0 :type fixnum)
  (content-length +max-content-length+ :type pointer)

  (http-major 0 :type http-version)
  (http-minor 0 :type http-version)
  (status-code nil :type (or null status-code))
  (method nil :type symbol)
  (http-errno +ok+ :type fixnum)

  (upgrade t :type boolean)
  (mark (make-mark)))

(declaim (inline parser-chunked-p parser-upgrade-p))

(defun parser-chunked-p (parser)
  (declare (optimize (speed 3) (safety 0)))
  (not (zerop (logand (parser-flags parser) +flag-chunked+))))

(defun parser-upgrade-p (parser)
  (declare (optimize (speed 3) (safety 0)))
  (not (zerop (logand (parser-flags parser) +flag-upgrade+))))

(defun parser-state-name (parser)
  (princ-to-string (aref +state-map+ (parser-state parser))))

(defstruct ll-callbacks
  (message-begin nil :type (or null function))     ;; 1 arg
  (url nil :type (or null function))
  (first-line nil :type (or null function))
  (status nil :type (or null function))
  (header-field nil :type (or null function))
  (header-value nil :type (or null function))
  (headers-complete nil :type (or null function))  ;; 1 arg
  (body nil :type (or null function))
  (message-complete nil :type (or null function))) ;; 1 arg

(defstruct mark
  (status nil :type (or null pointer))
  (url nil :type (or null pointer))
  (header-field nil :type (or null pointer))
  (header-value nil :type (or null pointer))
  (body nil :type (or null pointer)))

(declaim (inline init-mark))
(defun init-mark (mark)
  "Initialize all slots of MARK. This is for reusing the instance in a parser."
  (declare (optimize (speed 3) (safety 0)))
  (setf (mark-status mark) nil
        (mark-url mark) nil
        (mark-header-field mark) nil
        (mark-header-value mark) nil
        (mark-body mark) nil)
  mark)


;;
;; tokens & unhex maps

(define-constant +tokens+
    #( #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul   #\!   #\Nul   #\#    #\$    #\%    #\&    #\'
       #\Nul  #\Nul   #\*    #\+   #\Nul    #\-   #\.   #\Nul
        #\0    #\1    #\2    #\3    #\4    #\5    #\6    #\7
        #\8    #\9   #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul   #\a    #\b    #\c    #\d    #\e    #\f    #\g
        #\h    #\i    #\j    #\k    #\l    #\m    #\n    #\o
        #\p    #\q    #\r    #\s    #\t    #\u    #\v    #\w
        #\x    #\y    #\z   #\Nul  #\Nul  #\Nul   #\^    #\_
        #\`    #\a    #\b    #\c    #\d    #\e    #\f    #\g
        #\h    #\i    #\j    #\k    #\l    #\m    #\n    #\o
        #\p    #\q    #\r    #\s    #\t    #\u    #\v    #\w
        #\x    #\y    #\z   #\Nul   #\|   #\Nul   #\~   #\Nul )
  :test 'equalp)

(define-constant +unhex+
    #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
       0  1  2  3  4  5  6  7  8  9 -1 -1 -1 -1 -1 -1
      -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
  :test 'equalp)


;;
;; Flags

#.`(progn
     ,@(loop for i from 0
             for flag in '(chunked
                           trailing
                           upgrade
                           skipbody)
             collect `(defconstant ,(intern (format nil "+FLAG-~A+" flag)) ,(ash 1 i))))


;;
;; Functions

(declaim (inline parsing-header-p
                 http-message-needs-eof-p
                 init-mark))

(defun parsing-header-p (state)
  (declare (type fixnum state))
  (<= state +state-headers-done+))

(defun http-message-needs-eof-p (parser)
  (declare (optimize (speed 3) (safety 2)))
  (when (eq (parser-type parser) :request)
    (return-from http-message-needs-eof-p nil))

  (let ((status-code (parser-status-code parser)))
    (declare (type status-code status-code))
    ;; See RFC 2616 section 4.4
    (when (or (< 99 status-code 200) ;; 1xx e.g. Continue
              (= status-code 204)    ;; No Content
              (= status-code 304)    ;; Not Modified
              (not (zerop (logand (parser-flags parser) ;; response to a HEAD request
                                  +flag-skipbody+))))
      (return-from http-message-needs-eof-p nil)))

  (when (or (parser-chunked-p parser)
            (not (= (parser-content-length parser) +max-content-length+)))
    (return-from http-message-needs-eof-p nil))

  T)

(defmacro cond-new-message (parser start-req-form start-res-form dead-form)
  (declare (ignore dead-form))
  `(if (eq (parser-type ,parser) :request)
       ,start-req-form
       ,start-res-form))

(defun new-message (parser)
  (cond-new-message parser +state-start-req+ +state-start-res+ +state-dead+))

(defmacro callback-notify (parser callbacks callback-name)
  (with-gensyms (callback e)
    `(when-let (,callback (,(intern (format nil "~A-~A" :ll-callbacks callback-name)) ,callbacks))
       (handler-bind ((error
                        (lambda (,e)
                          (unless (typep ,e 'fast-http-error)
                            (error ',(intern (format nil "~A-~A" :cb callback-name))
                                   :error ,e)
                            (abort ,e)))))
         (values (funcall ,callback ,parser) T)))))

(defmacro callback-data (parser callbacks callback-name data mark end)
  (with-gensyms (callback start e)
    `(when-let (,start (,(intern (format nil "~A-~A" :mark callback-name)) ,mark))
       (when-let (,callback (,(intern (format nil "~A-~A" :ll-callbacks callback-name)) ,callbacks))
         (handler-bind ((error
                          (lambda (,e)
                            (unless (typep ,e 'fast-http-error)
                              (error ',(intern (format nil "~A-~A" :cb callback-name))
                                     :error ,e)
                              (abort ,e)))))
           (funcall ,callback ,parser ,data ,start ,end)))
       (setf (,(intern (format nil "~A-~A" :mark callback-name)) ,mark) nil))))

(defmacro set-mark (mark name val)
  `(unless (,(intern (format nil "~A-~A" :mark name)) ,mark)
     (setf (,(intern (format nil "~A-~A" :mark name)) ,mark) ,val)))

(declaim (inline check-header-overflow))
(defun check-header-overflow (header-read)
  (declare (type fixnum header-read)
           (optimize (speed 3) (safety 0)))
  (when (< +http-max-header-size+ header-read)
    (error 'header-overflow)))


;;
;; Macros for only inside of http-parse-headers & http-parse

(defmacro go-state (state &optional (advance 1) (set-state t))
  `(progn
     ,@(and set-state
            `((setf (parser-state parser) ,state)))
     ,(cond
        ((= advance 0) '())
        ((= advance 1) '(incf p))
        (T `(incf p ,advance)))
     ,@(unless (zerop advance)
         `((when (= p end)
             (go exit-loop))
           ,@(and (parsing-header-p (symbol-value state))
                  `((check-header-overflow (incf (parser-header-read parser)))))
           (setq byte (aref data p))
           #+fast-http-debug
           (log:debug (code-char byte))
           #+fast-http-debug
           (log:debug ,(princ-to-string state))))
     (go ,state)))

(defmacro check-header-field-end ()
  `(when (= byte #.(char-code #\:))
     (setf (parser-state parser) +state-header-value-discard-ws+)
     (callback-data parser callbacks :header-field
                    data mark p)
     (go-state +state-header-value-discard-ws+ 1 nil)))

(defmacro check-header-value-end ()
  `(casev= byte
     (+cr+
      (setf (parser-state parser) +state-header-almost-done+)
      (callback-data parser callbacks :header-value
                     data mark p)
      (go-state +state-header-almost-done+ 1 nil))
     (+lf+
      (setf (parser-state parser) +state-header-almost-done+)
      (callback-data parser callbacks :header-value
                     data mark p)
      (go-state +state-header-almost-done+ 0 nil))))

(defmacro go-header-field-state (state)
  `(progn
     (setf (parser-header-state parser) ,state)
     (incf p)
     (when (= p end)
       (go exit-loop))
     (check-header-overflow (incf (parser-header-read parser)))
     (setq byte (aref data p))
     (check-header-field-end)
     (setq char (aref +tokens+ byte))
     (when (char= char #\Nul)
       (error 'invalid-header-token))
     (go ,state)))

(defmacro go-header-value-state (state)
  `(progn
     (setf (parser-header-state parser) ,state)
     (incf p)
     (when (= p end)
       (go exit-loop))
     (check-header-overflow (incf (parser-header-read parser)))
     (setq byte (aref data p))
     (check-header-value-end)
     (go ,state)))

(defmacro looking-for (parser char string state &optional value)
  (let* ((string (etypecase string
                   (string string)
                   (symbol (symbol-value string))))
         (len (length string)))
    `(cond
       ((or (< ,len (parser-index ,parser))
            (not (char= ,char (aref ,string (parser-index ,parser)))))
        (,(if value
              'go-header-value-state
              'go-header-field-state) +header-state-general+))
       ((= (parser-index ,parser) ,(- len 1))
        (,(if value
              'go-header-value-state
              'go-header-field-state) ,state)))))


;;
;; Macros for URL parsing

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

(defmacro go-with-parse-url-char (state byte)
  `(progn
     (cond
       ((or (= ,byte +space+)
            (= ,byte +cr+)
            (= ,byte +lf+))
        (error 'invalid-url))
       ;; when strict mode
       #+fast-http-strict
       ((or (= ,byte +tab+)
            (= ,byte +page+))
        (error 'invalid-url))
       (T
        ,(case state
           (+state-req-spaces-before-url+
            ;; Proxied requests are followed by scheme of an absolute URI (alpha).
            ;; All methods except CONNECT are followed by '/' or '*'.
            `(cond
               ((or (= ,byte #.(char-code #\/))
                    (= ,byte #.(char-code #\*)))
                (go-state +state-req-path+))
               ((alpha-byte-char-p ,byte)
                (go-state +state-req-schema+))))
           (+state-req-schema+
            `(cond
               ((alpha-byte-char-p ,byte)
                (go-state ,state))
               ((= ,byte #.(char-code #\:))
                (go-state +state-req-schema-slash+))))
           (+state-req-schema-slash+
            `(when (= ,byte #.(char-code #\/))
               (go-state +state-req-schema-slash-slash+)))
           (+state-req-schema-slash-slash+
            `(when (= ,byte #.(char-code #\/))
               (go-state +state-req-server-start+)))
           ((+state-req-server-with-at+
             +state-req-server-start+
             +state-req-server+)
            `(cond
               ((and (= ,state +state-req-server-with-at+)
                     (= ,byte #.(char-code #\@)))
                (error 'invalid-url))
               ((= ,byte #.(char-code #\/))
                (go-state +state-req-path+))
               ((= ,byte #.(char-code #\?))
                (go-state +state-req-query-string-start+))
               ((= ,byte #.(char-code #\@))
                (go-state +state-req-server-with-at+))
               ((or (userinfo-byte-char-p ,byte)
                    (= ,byte #.(char-code #\[))
                    (= ,byte #.(char-code #\])))
                (go-state +state-req-server+))))
           (+state-req-path+
            `(cond
               ((= ,byte #.(char-code #\?))
                (go-state +state-req-query-string-start+))
               ((= ,byte #.(char-code #\#))
                (go-state +state-req-fragment-start+))
               ((<= #.(char-code #\!) ,byte #.(char-code #\~))
                (go-state ,state))
               ;; utf-8 path
               ((<= 128 ,byte) (go-state ,state))))
           ((+state-req-query-string-start+
             +state-req-query-string+)
            `(cond
               ((= ,byte #.(char-code #\?))
                ;; allow extra '?' in query string
                (go-state +state-req-query-string+))
               ((= ,byte #.(char-code #\#))
                (go-state +state-req-fragment-start+))
               ((<= #.(char-code #\!) ,byte #.(char-code #\~))
                (go-state +state-req-query-string+))))
           (+state-req-fragment-start+
            `(cond
               ((= ,byte #.(char-code #\?))
                (go-state +state-req-fragment+))
               ((= ,byte #.(char-code #\#))
                (go-state ,state))
               ((<= #.(char-code #\!) ,byte #.(char-code #\~))
                (go-state +state-req-fragment+))))
           (+state-req-fragment+
            `(when (<= #.(char-code #\!) ,byte #.(char-code #\~))
               (go-state ,state))))))
     (error 'invalid-url)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun state-req-absurl-case (state)
    `(,state (go-with-parse-url-char ,state byte)))

  (defun state-req-url-case (state)
    `(,state
      (casev= byte
        (+space+
         (setf (parser-state parser) +state-req-http-start+)
         (callback-data parser callbacks :url
                        data mark p)
         (go-state +state-req-http-start+ 1 nil))
        ((+cr+ +lf+)
         (setf (parser-http-major parser) 0
               (parser-http-minor parser) 9)
         (setf (parser-state parser)
               (if (= byte +cr+)
                   +state-req-line-almost-done+
                   +state-header-field-start+))
         (callback-data parser callbacks :url
                        data mark p)
         (callback-notify parser callbacks :first-line)
         (if (= byte +cr+)
             (go-state +state-req-line-almost-done+ 1 nil)
             (go-state +state-header-field-start+ 1 nil)))
        (otherwise
         (go-with-parse-url-char ,state byte))))))


;;
;; Main

(defun http-parse-headers (parser callbacks data &key (start 0) end)
  (declare (type simple-byte-vector data)
           (type pointer start)
           (optimize (speed 3) (safety 0)))
  (let* ((end (or end (length data)))
         (mark (init-mark (parser-mark parser)))
         (p start)
         (byte (aref data p)))
    (declare (type pointer end p)
             (type (unsigned-byte 8) byte))

    (tagbody
       (tagcasev= (parser-state parser)

         (+state-header-field-start+
          (casev= byte
            (+cr+
             (go-state +state-headers-almost-done+))
            (+lf+
             ;; they might be just sending \n instead of \r\n so this would be
             ;; the second \n to denote the end of headers
             (go-state +state-headers-almost-done+ 0))
            (otherwise
             (let ((char (aref +tokens+ byte)))
               (declare (type character char))
               (when (char= char #\Nul)
                 (error 'invalid-header-token))

               (set-mark mark :header-field p)
               (setf (parser-index parser) 0)

               (setf (parser-header-state parser)
                     (cond
                       ((char= char #\c)
                        +header-state-matching-content-length+)
                       ((char= char #\t)
                        +header-state-matching-transfer-encoding+)
                       ((char= char #\u)
                        +header-state-matching-upgrade+)
                       (T
                        +header-state-general+)))
               (go-state +state-header-field+)))))

         (+state-header-field+
          (check-header-field-end)
          (let ((char (aref +tokens+ byte)))
            (declare (type character char))
            (cond
              ((char= char #\Nul)
               (error 'invalid-header-token))
              (T
               (tagcasev= (parser-header-state parser)
                 (+header-state-general+
                  (go-header-field-state +header-state-general+))
                 (+header-state-matching-content-length+
                  (incf (parser-index parser))
                  (looking-for parser (aref +tokens+ byte) +content-length+ +header-state-content-length+)
                  (go-header-field-state +header-state-matching-content-length+))
                 (+header-state-matching-transfer-encoding+
                  (incf (parser-index parser))
                  (looking-for parser (aref +tokens+ byte) +transfer-encoding+ +header-state-transfer-encoding+)
                  (go-header-field-state +header-state-matching-transfer-encoding+))
                 (+header-state-matching-upgrade+
                  (incf (parser-index parser))
                  (looking-for parser (aref +tokens+ byte) +upgrade+ +header-state-upgrade+)
                  (go-header-field-state +header-state-matching-upgrade+))
                 (+header-state-content-length+
                  (unless (= byte +space+)
                    (go-header-field-state +header-state-general+))
                  (go-header-field-state +header-state-content-length+))
                 (+header-state-transfer-encoding+
                  (unless (= byte +space+)
                    (go-header-field-state +header-state-general+))
                  (go-header-field-state +header-state-transfer-encoding+))
                 (+header-state-upgrade+
                  (unless (= byte +space+)
                    (go-header-field-state +header-state-general+))
                  (go-header-field-state +header-state-upgrade+)))))))

         ((+state-header-value-discard-ws+
           +state-header-value-start+)
          (when +state-header-value-discard-ws+
            (casev= byte
              ((+space+ +tab+)
               (go-state +state-header-value-discard-ws+ 1 nil))
              (+cr+
               (go-state +state-header-value-discard-ws-almost-done+))
              (+lf+
               (go-state +state-header-value-discard-lws+))))

          (set-mark mark :header-value p)

          (setf (parser-index parser) 0)

          (casev= (parser-header-state parser)
            (+header-state-upgrade+
             (setf (parser-flags parser)
                   (logxor (parser-flags parser) +flag-upgrade+))
             (setf (parser-header-state parser) +header-state-general+)
             (go-state +state-header-value+))
            (+header-state-transfer-encoding+
             ;; looking for 'Transfer-Encoding: chunked'
             (setf (parser-header-state parser)
                   (if (or (= byte #.(char-code #\c))
                           (= byte #.(char-code #\C)))
                       +header-state-matching-transfer-encoding-chunked+
                       +header-state-general+))
             (go-state +state-header-value+))
            (+header-state-content-length+
             (unless (digit-byte-char-p byte)
               (error 'invalid-content-length))
             (setf (parser-content-length parser)
                   (digit-byte-char-to-integer byte))
             (go-state +state-header-value+))
            (otherwise
             (setf (parser-header-state parser) +header-state-general+)
             (go-state +state-header-value+))))
         (+state-header-value+
          (check-header-value-end)
          (tagcasev= (parser-header-state parser)
            (+header-state-general+
             (go-header-value-state +header-state-general+))
            (+header-state-transfer-encoding+
             (go-header-value-state +header-state-transfer-encoding+))
            (+header-state-content-length+
             (unless (= byte +space+)
               (unless (digit-byte-char-p byte)
                 (error 'invalid-content-length))
               (setf (parser-content-length parser)
                     (+ (* 10 (parser-content-length parser))
                        (digit-byte-char-to-integer byte))))
             (go-header-value-state +header-state-content-length+))
            ;; Transfer-Encoding: chunked
            (+header-state-matching-transfer-encoding-chunked+
             (incf (parser-index parser))
             (looking-for parser (alpha-byte-char-to-lower-char byte)
                          +chunked+ +header-state-transfer-encoding-chunked+ T)
             (go-header-value-state +header-state-matching-transfer-encoding-chunked+))
            (+header-state-transfer-encoding-chunked+
             (unless (= byte +space+)
               (setf (parser-header-state parser) +header-state-general+))
             (go-header-value-state +header-state-transfer-encoding-chunked+))
            (otherwise
             (go-header-value-state +header-state-general+))))

         (+state-header-almost-done+
          (check-strictly (= byte +lf+))
          (go-state +state-header-value-lws+))

         (+state-header-value-lws+
          (when (or (= byte +space+)
                    (= byte +tab+))
            (set-mark mark :header-value p)
            (go-state +state-header-value-start+ 0))
          ;; finished the header
          (casev= (parser-header-state parser)
            (+header-state-transfer-encoding-chunked+
             (setf (parser-flags parser)
                   (logxor (parser-flags parser) +flag-chunked+))))
          (go-state +state-header-field-start+ 0))

         (+state-header-value-discard-ws-almost-done+
          (check-strictly (= byte +lf+))
          (go-state +state-header-value-discard-lws+))

         (+state-header-value-discard-lws+
          (if (or (= byte +space+)
                  (= byte +tab+))
              (go-state +state-header-value-discard-ws+)
              (progn
                ;; header value was empty
                (set-mark mark :header-value p)
                (setf (parser-state parser) +state-header-field-start+)
                (callback-data parser callbacks :header-value
                               data mark p)
                (go-state +state-header-field-start+ 0 nil))))

         (+state-headers-almost-done+
          (check-strictly (= byte +lf+))
          (setf (parser-header-read parser) 0)
          (go exit-loop)))
     exit-loop)
    p))

(defun http-parse (parser callbacks data &key (start 0) end)
  (declare (type simple-byte-vector data)
           (type pointer start)
           (optimize (speed 3) (safety 0)))
  (let ((end (or end (length data)))
        (mark (init-mark (parser-mark parser))))
    (declare (type pointer end))

    (unless (= (parser-http-errno parser) +ok+)
      (return-from http-parse start))

    (when (= start end)
      (casev= (parser-state parser)
        (+state-body-identity-eof+
         (callback-notify parser callbacks :message-complete)
         (return-from http-parse start))
        ((+state-dead+
          +state-start-req-or-res+
          +state-start-res+
          +state-start-req+)
         (return-from http-parse start))
        (otherwise
         (error 'invalid-eof-state))))

    (macrolet ((with-new-message (parser &body form)
                 `(cond-new-message ,parser
                                    (progn
                                      (setf (parser-state ,parser) +state-start-req+)
                                      ,@form
                                      (go-state +state-start-req+))
                                    (progn
                                      (setf (parser-state ,parser) +state-start-res+)
                                      ,@form
                                      (go-state +state-start-res+))
                                    (progn
                                      ,@form
                                      (go-state +state-dead+)))))
      (tagbody
         (do ((p start (1+ p)))
             ((= p end))
           (declare (type pointer p))
           (let ((byte (aref data p)))
             #+fast-http-debug
             (log:debug (code-char byte))
             #+fast-http-debug
             (log:debug (parser-state-name parser))
             (when (parsing-header-p (parser-state parser))
               (check-header-overflow (incf (parser-header-read parser))))

             (tagcasev= (parser-state parser)

               (+state-dead+
                (unless (or (= byte +cr+)
                            (= byte +lf+))
                  (error 'closed-connection)))

               (+state-start-req-or-res+
                (unless (or (= byte +cr+)
                            (= byte +lf+))
                  (setf (parser-flags parser) 0
                        (parser-content-length parser) +max-content-length+)

                  (if (= byte #.(char-code #\H))
                      (progn
                        (setf (parser-state parser) +state-res-or-resp-H+)
                        (callback-notify parser callbacks :message-begin)
                        (go-state +state-res-or-resp-H+ 1 nil))
                      (progn
                        (setf (parser-type parser) :request)
                        (go-state +state-start-req+ 0)))))

               (+state-res-or-resp-H+
                (case-byte byte
                  (#\T
                   (setf (parser-type parser) :response)
                   (go-state +state-res-HT+))
                  (#\E
                   (setf (parser-type parser) :request
                         (parser-method parser) :head
                         (parser-index parser) 2
                         (parser-state parser) +state-req-method+)
                   (go-state +state-req-method+))
                  (otherwise
                   (error 'invalid-constant))))

               (+state-start-res+
                (setf (parser-flags parser) 0
                      (parser-content-length parser) +max-content-length+)

                (case-byte byte
                  (#\H
                   (setf (parser-state parser) +state-res-H+)
                   (callback-notify parser callbacks :message-begin)
                   (go-state +state-res-H+ 1 nil))
                  ((#\Return #\Newline)
                   (go-state +state-start-res+))
                  (otherwise
                   (error 'invalid-constant))))

               (+state-res-H+
                (check-strictly (= byte #.(char-code #\T)))
                (go-state +state-res-HT+))

               (+state-res-HT+
                (check-strictly (= byte #.(char-code #\T)))
                (go-state +state-res-HTT+))

               (+state-res-HTT+
                (check-strictly (= byte #.(char-code #\P)))
                (go-state +state-res-HTTP+))

               (+state-res-HTTP+
                (check-strictly (= byte #.(char-code #\/)))
                (go-state +state-res-first-http-major+))

               (+state-res-first-http-major+
                (unless (digit-byte-char-p byte)
                  (error 'invalid-version))

                (setf (parser-http-major parser) (digit-byte-char-to-integer byte))
                (go-state +state-res-http-major+))

               (+state-res-http-major+
                (cond
                  ((= byte #.(char-code #\.))
                   (go-state +state-res-first-http-minor+))
                  ((not (digit-byte-char-p byte))
                   (error 'invalid-version))
                  (T
                   (let ((major (parser-http-major parser)))
                     (setq major
                           (+ (* major 10) (digit-byte-char-to-integer byte)))

                     (when (< 999 major)
                       (error 'invalid-version))

                     (setf (parser-http-major parser) major)))))

               (+state-res-first-http-minor+
                (unless (digit-byte-char-p byte)
                  (error 'invalid-version))
                (setf (parser-http-minor parser) (digit-byte-char-to-integer byte))
                (go-state +state-res-http-minor+))

               (+state-res-http-minor+
                (cond
                  ((= byte #.(char-code #\Space))
                   (go-state +state-res-first-status-code+))
                  ((not (digit-byte-char-p byte))
                   (error 'invalid-version))
                  (T
                   (let ((minor (parser-http-minor parser)))
                     (setq minor
                           (+ (* minor 10)
                              (digit-byte-char-to-integer byte)))

                     (when (< 999 minor)
                       (error 'invalid-version))

                     (setf (parser-http-minor parser) minor)))))

               (+state-res-first-status-code+
                (unless (or (digit-byte-char-p byte)
                            (= byte #.(char-code #\Space)))
                  (error 'invalid-status))
                (setf (parser-status-code parser) (digit-byte-char-to-integer byte))
                (go-state +state-res-status-code+))

               (+state-res-status-code+
                (cond
                  ((not (digit-byte-char-p byte))
                   (case-byte byte
                     (#\Space
                      (go-state +state-res-status-start+))
                     (#\Return
                      (go-state +state-res-line-almost-done+))
                     (#\Newline
                      (go-state +state-header-field-start+))
                     (otherwise
                      (error 'invalid-status))))
                  (T
                   (let ((status-code (parser-status-code parser)))
                     (setq status-code
                           (+ (* 10 status-code)
                              (digit-byte-char-to-integer byte)))
                     (when (< 999 status-code)
                       (error 'invalid-status :status-code status-code))
                     (setf (parser-status-code parser) status-code)))))

               (+state-res-status-start+
                (casev= byte
                  (+cr+
                   (go-state +state-res-line-almost-done+))
                  (+lf+
                   (go-state +state-header-field-start+))
                  (otherwise
                   (set-mark mark :status p)
                   (setf (parser-index parser) 0)
                   (go-state +state-res-status+))))

               (+state-res-status+
                (casev= byte
                  (+cr+
                   (setf (parser-state parser) +state-res-line-almost-done+)
                   (callback-data parser callbacks :status
                                  data mark p)
                   (go-state +state-res-line-almost-done+ 1 nil))
                  (+lf+
                   (setf (parser-state parser) +state-header-field-start+)
                   (callback-data parser callbacks :status
                                  data mark p)
                   (callback-notify parser callbacks :first-line)
                   (go-state +state-header-field-start+ 1 nil))))

               (+state-res-line-almost-done+
                (check-strictly (= byte +lf+))
                (callback-notify parser callbacks :first-line)
                (go-state +state-header-field-start+))

               (+state-start-req+
                (unless (or (= byte +cr+)
                            (= byte +lf+))
                  (setf (parser-flags parser) 0
                        (parser-content-length parser) +max-content-length+)

                  (unless (alpha-byte-char-p byte)
                    (error 'invalid-method))

                  (setf (parser-index parser) 1)

                  (setf (parser-method parser)
                        (case-byte byte
                          (#\C :connect)
                          (#\D :delete)
                          (#\G :get)
                          (#\H :head)
                          (#\L :lock)
                          (#\M :mkcol) ;; or MOVE, MKACTIVITY, MERGE, M-SEARCH, MKCALENDAR
                          (#\N :notify)
                          (#\O :options)
                          (#\P :post) ;; or PROPFIND, PROPPATCH, PUT, PATCH, PURGE
                          (#\R :report)
                          (#\S :subscribe) ;; or SEARCH
                          (#\T :trace)
                          (#\U :unlock) ;; or UNSUBSCRIBE
                          (otherwise
                           (error 'invalid-method))))
                  (setf (parser-state parser) +state-req-method+)
                  (callback-notify parser callbacks :message-begin)
                  (go-state +state-req-method+ 1 nil)))

               (+state-req-method+
                (let ((matcher (symbol-name (parser-method parser)))
                      (method (parser-method parser)))
                  (cond
                    ((and (= byte +space+)
                          (<= (length matcher) (parser-index parser)))
                     (incf (parser-index parser))
                     (go-state +state-req-spaces-before-url+))
                    ((and (< (parser-index parser) (length matcher))
                          (= byte (char-code (aref matcher (parser-index parser)))))
                     (incf (parser-index parser))
                     (go-state +state-req-method+))
                    ((eq method :connect)
                     (cond
                       ((and (= (parser-index parser) 1)
                             (= byte #.(char-code #\H)))
                        (setf (parser-method parser) :checkout))
                       ((and (= (parser-index parser) 2)
                             (= byte #.(char-code #\P)))
                        (setf (parser-method parser) :copy))
                       (T
                        (error 'invalid-method))))
                    ((eq method :mkcol)
                     (case (parser-index parser)
                       (1
                        (setf (parser-method parser)
                              (case-byte byte
                                (#\O :move)
                                (#\E :merge)
                                (#\- :m-search)
                                (otherwise (error 'invalid-method)))))
                       (2
                        (unless (= byte #.(char-code #\A))
                          (error 'invalid-method))
                        (setf (parser-method parser) :mkactivity))
                       (3
                        (unless (= byte #.(char-code #\A))
                          (error 'invalid-method))
                        (setf (parser-method parser) :mkcalendar))
                       (otherwise
                        (error 'invalid-method))))
                    ((eq method :subscribe)
                     (cond
                       ((and (= (parser-index parser) 1)
                             (= byte #.(char-code #\E)))
                        (setf (parser-method parser) :search))
                       (T
                        (error 'invalid-method))))
                    ((and (= (parser-index parser) 1)
                          (eq method :post))
                     (setf (parser-method parser)
                           (case-byte byte
                             (#\R :propfind)
                             (#\U :put)
                             (#\A :patch)
                             (otherwise (error 'invalid-method)))))
                    ((= (parser-index parser) 2)
                     (cond
                       ((eq method :put)
                        (unless (= byte #.(char-code #\R))
                          (error 'invalid-method))
                        (setf (parser-method parser) :purge))
                       ((eq method :unlock)
                        (unless (= byte #.(char-code #\S))
                          (error 'invalid-method))
                        (setf (parser-method parser) :unsubscribe))
                       (T
                        (error 'invalid-method))))
                    ((and (= (parser-index parser) 4)
                          (eq (parser-method parser) :propfind)
                          (= byte #.(char-code #\P)))
                     (setf (parser-method parser) :proppatch))
                    (T
                     (error 'invalid-method)))

                  (incf (parser-index parser))))

               (+state-req-spaces-before-url+
                (when (= byte +space+)
                  (go-state +state-req-spaces-before-url+ 1 nil))
                (set-mark mark :url p)
                (if (eq (parser-method parser) :connect)
                    (go-with-parse-url-char +state-req-server-start+ byte)
                    (go-with-parse-url-char +state-req-spaces-before-url+ byte)))

               #.(state-req-absurl-case '+state-req-schema+)
               #.(state-req-absurl-case '+state-req-schema-slash+)
               #.(state-req-absurl-case '+state-req-schema-slash-slash+)
               #.(state-req-absurl-case '+state-req-server-start+)

               #.(state-req-url-case '+state-req-server+)
               #.(state-req-url-case '+state-req-server-with-at+)
               #.(state-req-url-case '+state-req-path+)
               #.(state-req-url-case '+state-req-query-string-start+)
               #.(state-req-url-case '+state-req-query-string+)
               #.(state-req-url-case '+state-req-fragment-start+)
               #.(state-req-url-case '+state-req-fragment+)

               (+state-req-http-start+
                (case-byte byte
                  (#\H
                   (go-state +state-req-http-H+))
                  (#\Space)
                  (otherwise
                   (error 'invalid-constant))))

               (+state-req-http-H+
                (check-strictly (= byte #.(char-code #\T)))
                (go-state +state-req-http-HT+))

               (+state-req-http-HT+
                (check-strictly (= byte #.(char-code #\T)))
                (go-state +state-req-http-HTT+))

               (+state-req-http-HTT+
                (check-strictly (= byte #.(char-code #\P)))
                (go-state +state-req-http-HTTP+))

               (+state-req-http-HTTP+
                (check-strictly (= byte #.(char-code #\/)))
                (go-state +state-req-first-http-major+))

               (+state-req-first-http-major+
                (unless (and (digit-byte-char-p byte)
                             (not (= byte #.(char-code #\0))))
                  (error 'invalid-version))
                (setf (parser-http-major parser) (digit-byte-char-to-integer byte))
                (go-state +state-req-http-major+))

               (+state-req-http-major+
                (cond
                  ((= byte #.(char-code #\.))
                   (go-state +state-req-first-http-minor+))
                  ((not (digit-byte-char-p byte))
                   (error 'invalid-version))
                  (T
                   (let ((major (parser-http-major parser)))
                     (setq major
                           (+ (* 10 major)
                              (digit-byte-char-to-integer byte)))
                     (when (< 999 major)
                       (error 'invalid-version))
                     (setf (parser-http-major parser) major)))))

               (+state-req-first-http-minor+
                (unless (digit-byte-char-p byte)
                  (error 'invalid-version))

                (setf (parser-http-minor parser) (digit-byte-char-to-integer byte))
                (go-state +state-req-http-minor+))

               (+state-req-http-minor+
                (cond
                  ((= byte +cr+)
                   (callback-notify parser callbacks :first-line)
                   (go-state +state-req-line-almost-done+))
                  ((= byte +lf+)
                   (callback-notify parser callbacks :first-line)
                   (go-state +state-header-field-start+))
                  ((not (digit-byte-char-p byte))
                   (error 'invalid-version))
                  (T
                   (let ((minor (parser-http-minor parser)))
                     (setq minor
                           (+ (* 10 minor)
                              (digit-byte-char-to-integer byte)))
                     (when (< 999 minor)
                       (error 'invalid-version))
                     (setf (parser-http-minor parser) minor)))))
               (+state-req-line-almost-done+
                (unless (= byte +lf+)
                  (error 'lf-expected))

                (go-state +state-header-field-start+))

               ((+state-header-field-start+
                 +state-header-field+
                 +state-header-value-discard-ws+
                 +state-header-value-discard-ws-almost-done+
                 +state-header-value-discard-lws+
                 +state-header-value-start+
                 +state-header-value+
                 +state-header-value-lws+)
                (let ((next (http-parse-headers parser callbacks data :start p :end end)))
                  (when (= (the pointer next) end)
                    (go exit-loop))
                  (setq p next)
                  (go-state +state-headers-almost-done+ 0 nil)))

               (+state-headers-almost-done+
                (cond
                  ((not (zerop (logand (parser-flags parser) +flag-trailing+)))
                   ;; End of a chunked request
                   (with-new-message parser
                     (callback-notify parser callbacks :message-complete)))
                  (T
                   (setf (parser-state parser) +state-headers-done+)

                   ;; Set this here so that on_headers_complete() callbacks can see it
                   (setf (parser-upgrade parser)
                         (or (parser-upgrade-p parser)
                             (eq (parser-method parser) :connect)))

                   (multiple-value-bind (retval existsp)
                       (callback-notify parser callbacks :headers-complete)
                     (when (and existsp
                                (eq retval :skipbody))
                       (setf (parser-flags parser)
                             (logxor (parser-flags parser) +flag-skipbody+))))

                   (go-state +state-headers-done+ 0 nil))))

               (+state-headers-done+
                ;; Exit, the rest of the connect is in a different protocol.
                (when (parser-upgrade parser)
                  (setf (parser-state parser) (new-message parser))
                  (callback-notify parser callbacks :message-complete)
                  (return-from http-parse (1+ p)))

                (cond
                  ((not (zerop (logand (parser-flags parser) +flag-skipbody+)))
                   (with-new-message parser
                     (callback-notify parser callbacks :message-complete)))
                  ((parser-chunked-p parser)
                   ;; chunked encoding - ignore Content-Length header
                   (go-state +state-chunk-size-start+))
                  (T
                   (casev= (parser-content-length parser)
                     (0
                      ;; Content-Length header given but zero: Content-Length: 0\r\n
                      (with-new-message parser
                        (callback-notify parser callbacks :message-complete)))
                     (+max-content-length+
                      (cond
                        ((or (eq (parser-type parser) :request)
                             (not (http-message-needs-eof-p parser)))
                         ;; Assume content-length 0 - read the next
                         (with-new-message parser
                           (callback-notify parser callbacks :message-complete)))
                        (T (go-state +state-body-identity-eof+))))
                     (otherwise
                      ;; Content-Length header given and non-zero
                      (go-state +state-body-identity+))))))

               (+state-body-identity+
                (let ((to-read (min (the pointer (parser-content-length parser))
                                    (the pointer (- end p)))))
                  (assert (and (not (= (parser-content-length parser) 0))
                               (not (= (parser-content-length parser) +max-content-length+))))

                  (set-mark mark :body p)
                  (decf (parser-content-length parser) to-read)
                  (incf p (1- to-read))

                  (when (zerop (parser-content-length parser))
                    (setf (parser-state parser) +state-message-done+)
                    (callback-data parser callbacks :body
                                   data mark (1+ p))
                    (go-state +state-message-done+ 0 nil))))
               ;; read until EOF
               (+state-body-identity-eof+
                (set-mark mark :body p)
                (setq p (- end 1)))
               (+state-message-done+
                (with-new-message parser
                  (callback-notify parser callbacks :message-complete)))
               (+state-chunk-size-start+
                (assert (= (parser-header-read parser) 1))
                (assert (parser-chunked-p parser))

                (let ((unhex-val (aref +unhex+ byte)))
                  (declare (type fixnum unhex-val))
                  (when (= unhex-val -1)
                    (error 'invalid-chunk-size))
                  (setf (parser-content-length parser) unhex-val)
                  (go-state +state-chunk-size+)))
               (+state-chunk-size+
                (assert (parser-chunked-p parser))

                (cond
                  ((= byte +cr+)
                   (go-state +state-chunk-size-almost-done+))
                  (T
                   (let ((unhex-val (aref +unhex+ byte)))
                     (declare (type fixnum unhex-val))
                     (cond
                       ((= unhex-val -1)
                        (case-byte byte
                          ((#\; #\Space)
                           (go-state +state-chunk-parameters+))
                          (otherwise
                           (error 'invalid-chunk-size))))
                       (T
                        (when (< #.(/ (- (expt 2 64) 1 16) 16) ;; = (/ (- +max-content-length+ 16) 16)
                                 (parser-content-length parser))
                          (error 'invalid-content-length))
                        (setf (parser-content-length parser)
                              (+ (* 16 (parser-content-length parser)) unhex-val))
                        (go-state +state-chunk-size+)))))))
               (+state-chunk-parameters+
                (assert (parser-chunked-p parser))
                ;; just ignore this shit.
                (when (= byte +cr+)
                  (go-state +state-chunk-size-almost-done+))
                (go-state +state-chunk-parameters+))
               (+state-chunk-size-almost-done+
                (assert (parser-chunked-p parser))
                (check-strictly (= byte +lf+))
                (setf (parser-header-read parser) 0)

                (cond
                  ((zerop (parser-content-length parser))
                   (setf (parser-flags parser)
                         (logxor (parser-flags parser) +flag-trailing+))
                   (go-state +state-header-field-start+))
                  (T
                   (go-state +state-chunk-data+))))
               (+state-chunk-data+
                (let ((to-read (min (the pointer (parser-content-length parser))
                                    (the pointer (- end p)))))
                  (assert (parser-chunked-p parser))
                  (assert (and (not (zerop (parser-content-length parser)))
                               (not (= (parser-content-length parser) +max-content-length+))))

                  (set-mark mark :body p)
                  (decf (parser-content-length parser) to-read)
                  (incf p (1- to-read))
                  (when (zerop (parser-content-length parser))
                    (go-state +state-chunk-data-almost-done+))
                  (go-state +state-chunk-data+)))
               (+state-chunk-data-almost-done+
                (assert (parser-chunked-p parser))
                (assert (zerop (parser-content-length parser)))
                (check-strictly (= byte +cr+))
                (setf (parser-state parser) +state-chunk-data-done+)
                (callback-data parser callbacks :body
                               data mark p)
                (go-state +state-chunk-data-done+ 1 nil))
               (+state-chunk-data-done+
                (assert (parser-chunked-p parser))
                (check-strictly (= byte +lf+))
                (setf (parser-header-read parser) 0)
                (go-state +state-chunk-size-start+))
               (otherwise
                (error 'invalid-internal-state :code (parser-state parser))))))
       exit-loop))

    (callback-data parser callbacks :header-field
                   data mark end)
    (callback-data parser callbacks :header-value
                   data mark end)
    (callback-data parser callbacks :url
                   data mark end)
    (callback-data parser callbacks :body
                   data mark end)
    (callback-data parser callbacks :status
                   data mark end)

    (return-from http-parse end)))

(declaim (notinline parsing-header-p
                    http-message-needs-eof-p
                    init-mark))


(defun parse-header-value-parameters (data &key
                                             header-value-callback
                                             header-parameter-key-callback
                                             header-parameter-value-callback)
  (declare (type simple-string data)
           (optimize (speed 3) (safety 2)))

  (let* ((header-name-mark 0)
         parameter-key-mark
         parameter-value-mark
         parsing-quoted-string-p
         (p 0)
         (end (length data))
         (char (aref data p)))
    (declare (type character char))

    (when (= end 0)
      (return-from parse-header-value-parameters 0))

    (macrolet ((go-state (state &optional (advance 1))
                   `(locally (declare (optimize (speed 3) (safety 0)))
                      (incf p ,advance)
                      (when (= p end)
                        (go eof))
                      (setq char (aref data p))
                      (go ,state))))
      (flet ((tokenp (char)
               (declare (optimize (speed 3) (safety 0)))
               (let ((byte (char-code char)))
                 (and (< byte 128)
                      (not (char= (the character (aref +tokens+ byte)) #\Nul))))))
        (tagbody
         parsing-header-value-start
           (case char
             ((#\Space #\Tab)
              (go-state parsing-header-value))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-header-value))
              (setq header-name-mark p)
              (go-state parsing-header-value 0)))

         parsing-header-value
           (case char
             (#\;
              (when header-value-callback
                (funcall (the function header-value-callback)
                         data header-name-mark p))
              (setq header-name-mark nil)
              (go-state looking-for-parameter-key))
             (otherwise (go-state parsing-header-value)))

         looking-for-parameter-key
           (case char
             ((#\Space #\Tab #\; #\Newline #\Return)
              (go-state looking-for-parameter-key))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-parameter-key))
              (setq parameter-key-mark p)
              (go-state parsing-parameter-key)))

         parsing-parameter-key
           (case char
             (#\=
              (assert parameter-key-mark)
              (when header-parameter-key-callback
                (funcall (the function header-parameter-key-callback)
                         data parameter-key-mark p))
              (setq parameter-key-mark nil)
              (go-state parsing-parameter-value-start))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-parameter-key))
              (go-state parsing-parameter-key)))

         parsing-parameter-value-start
           (case char
             (#\"
              ;; quoted-string
              (setq parameter-value-mark (1+ p))
              (setq parsing-quoted-string-p t)
              (go-state parsing-parameter-quoted-value))
             ((#.+space+ #.+tab+)
              (go-state parsing-parameter-value-start))
             (otherwise
              (setq parameter-value-mark p)
              (go-state parsing-parameter-value 0)))

         parsing-parameter-quoted-value
           (if (char= char #\")
               (progn
                 (assert parameter-value-mark)
                 (setq parsing-quoted-string-p nil)
                 (when header-parameter-value-callback
                   (funcall (the function header-parameter-value-callback)
                            data parameter-value-mark p))
                 (setq parameter-value-mark nil)
                 (go-state looking-for-parameter-key))
               (go-state parsing-parameter-quoted-value))

         parsing-parameter-value
           (case char
             (#\;
              (assert parameter-value-mark)
              (when header-parameter-value-callback
                (funcall (the function header-parameter-value-callback)
                         data parameter-value-mark p))
              (setq parameter-value-mark nil)
              (go-state looking-for-parameter-key))
             (otherwise
              (go-state parsing-parameter-value)))

         eof
           (when header-name-mark
             (when header-value-callback
               (funcall (the function header-value-callback)
                        data header-name-mark p)))
           (when parameter-key-mark
             (error 'invalid-eof-state))
           (when parameter-value-mark
             (when parsing-quoted-string-p
               (error 'invalid-eof-state))
             (when header-parameter-value-callback
               (funcall (the function header-parameter-value-callback)
                        data parameter-value-mark p))))))
    p))

(declaim (notinline parser-chunked-p parser-upgrade-p))
