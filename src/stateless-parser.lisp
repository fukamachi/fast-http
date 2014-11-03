(in-package :cl-user)
(defpackage fast-http.stateless-parser
  (:use :cl
        :fast-http.error)
  (:import-from :fast-http.byte-vector
                :+space+
                :+tab+
                :+cr+
                :+lf+
                :simple-byte-vector
                :alpha-byte-char-p
                :digit-byte-char-p
                :digit-byte-char-to-integer)
  (:import-from :fast-http.util
                :casev=
                :case-byte)
  (:import-from :alexandria
                :with-gensyms
                :hash-table-alist
                :define-constant
                :when-let)
  (:export :make-parser
           :make-callbacks
           :parser-type
           :parser-method
           :parser-status-code
           :parser-http-major
           :parser-http-minor
           :parser-content-length
           :parser-chunked-p
           :parser-upgrade-p
           :parse-request
           :parse-response
           :http-parse))
(in-package :fast-http.stateless-parser)

;;
;; Macro utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *insane-declaration* '(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))))
  (defvar *speedy-declaration* '(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))))
  (defvar *careful-declaration* '(declare (optimize (speed 3) (safety 2)))))

(defmacro defun-insane (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,*insane-declaration*
       ,@body)))

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list
       ,*speedy-declaration*
       ,@body)))

(defmacro defun-careful (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list
       ,*careful-declaration*
       ,@body)))


;;
;; Variables

(declaim (type fixnum +max-header-line+))
(defconstant +max-header-line+ 1024
  "Maximum number of header lines allowed.

This restriction is for protecting users' application
against denial-of-service attacks where the attacker feeds
us a never-ending header that the application keeps buffering.")


;;
;; Types

(deftype pointer () 'integer)
(deftype status-code () '(integer 0 10000))


;;
;; Parser declaration

(defstruct parser
  (type :both :type keyword)
  (method nil :type symbol)
  (http-major 0 :type fixnum)
  (http-minor 9 :type fixnum)
  (status-code 0 :type status-code)
  (content-length nil :type (or null integer))
  (chunked-p nil :type boolean)
  (upgrade-p nil :type boolean)

  ;; private
  (header-read 0 :type fixnum))


;;
;; Callbacks

(defstruct callbacks
  (message-begin nil :type (or null function))     ;; 1 arg
  (url nil :type (or null function))
  (first-line nil :type (or null function))
  (status nil :type (or null function))
  (header-field nil :type (or null function))
  (header-value nil :type (or null function))
  (headers-complete nil :type (or null function))  ;; 1 arg
  (body nil :type (or null function))
  (message-complete nil :type (or null function)))

(defmacro callback-data (name parser callbacks data start end)
  (with-gensyms (callback)
    `(when-let (,callback (,(intern (format nil "~A-~A" :callbacks name)) ,callbacks))
       (funcall ,callback ,parser ,data ,start ,end))))

(defmacro callback-notify (name parser callbacks)
  (with-gensyms (callback)
    `(when-let (,callback (,(intern (format nil "~A-~A" :callbacks name)) ,callbacks))
       (funcall ,callback ,parser))))


;;
;; Parser utilities

(define-condition eof ()
  ((pointer :initarg :pointer
            :initform nil
            :reader eof-pointer)))

(defmacro check-eof ()
  `(when (= p end)
     (error 'eof)))

(defmacro with-transaction ((&key var) &body body)
  (with-gensyms (start e)
    (flet ((main-part ()
             `(handler-case (progn ,@body)
                (eof (,e)
                  (if (eof-pointer ,e)
                      (error ,e)
                      (error 'eof :pointer ,(or var start)))))))
      (if var
          `(progn
             (setq ,var p)
             ,(main-part))
          `(let ((,start p))
             (declare (dynamic-extent ,start))
             ,(main-part))))))

(defmacro advance (&optional (degree 1))
  `(progn
     (incf p ,degree)
     (check-eof)
     (setq byte (aref data p))))

(defmacro advance-to (to)
  `(progn
     (setq p ,to)
     (check-eof)
     (setq byte (aref data p))))

(defmacro skip-while (form)
  `(loop while (progn ,form)
         do (advance)))

(defmacro skip-while-spaces ()
  '(skip-while (or (= byte +space+) (= byte +tab+))))

(defmacro skip-until (form)
  `(loop until (progn ,form)
         do (advance)))

(defmacro skip-until-crlf ()
  `(loop if (= byte +cr+)
           do (progn (expect-byte +lf+)
                     (return))
         else
           do (advance)))

(define-condition expect-failed (simple-error) ())

(defmacro expect (form &optional (error ''expect-failed) (advance T))
  `(progn
     ,@(and advance `((advance)))
     ,(if error
          `(unless (progn ,form)
             (error ,error))
          `(progn ,form))))

(defmacro expect-byte (byte &optional (error ''expect-failed) (advance T))
  "Advance the pointer and check if the next byte is BYTE."
  `(expect (= byte ,byte) ,error ,advance))

(defmacro expect-char (char &optional (error ''expect-failed) (advance T) (case-sensitive T))
  (if case-sensitive
      `(expect-byte ,(char-code char) ,error ,advance)
      `(expect (or (= byte ,(char-code char))
                   (= byte ,(if (lower-case-p char)
                                (- (char-code char) 32)
                                (+ (char-code char) 32))))
           ,error
           ,advance)))

(defmacro expect-string (string &optional (error ''expect-failed) (advance T) (case-sensitive T))
  (if advance
      `(progn
         ,@(loop for char across string
                 collect `(expect-char ,char ,error ,advance ,case-sensitive)))
      (when (/= 0 (length string))
        `(progn
           (expect-char ,(aref string 0) ,error nil ,case-sensitive)
           (expect-string ,(subseq string 1) ,error T ,case-sensitive)))))

(defmacro expect-crlf (&optional (error ''expect-failed))
  `(casev= byte
     (+cr+ (expect-byte +lf+ ,error))
     ,@(and error
            `((otherwise (error ,error))))))

(defmacro expect-one-of (strings &optional (error ''expect-failed) (case-sensitive T))
  (let ((expect-block (gensym "EXPECT-BLOCK")))
    (labels ((grouping (i strings)
               (let ((map (make-hash-table)))
                 (loop for string in strings
                       when (< 0 (- (length (string string)) i))
                         do (push string
                                  (gethash (aref (string string) i) map)))
                 (alexandria:hash-table-alist map)))
             (build-case-byte (i strings)
               (let ((alist (grouping i strings)))
                 (if alist
                     `(case-byte byte
                        ,@(loop for (char . candidates) in alist
                                if (cdr candidates)
                                  collect `(,(if case-sensitive
                                                 char
                                                 (if (lower-case-p char)
                                                     `(,char ,(code-char (- (char-code char) 32)))
                                                     `(,char ,(code-char (+ (char-code char) 32)))))
                                            (advance) ,(build-case-byte (1+ i) candidates))
                                else
                                  collect `(,(if case-sensitive
                                                 char
                                                 (if (lower-case-p char)
                                                     `(,char ,(code-char (- (char-code char) 32)))
                                                     `(,char ,(code-char (+ (char-code char) 32)))))
                                            (expect-string ,(subseq (string (car candidates)) (1+ i)) ,error T ,case-sensitive)
                                            (return-from ,expect-block ,(car candidates))))
                        (otherwise ,(if error
                                        `(error ,error)
                                        `(return-from ,expect-block nil))))
                     `(return-from ,expect-block ,(car strings))))))
      `(block ,expect-block
         ,(build-case-byte 0 strings)))))

(defmacro case-expect-header-field (strings &body clauses)
  (with-gensyms (expect-block)
    (labels ((grouping (i strings)
               (let ((map (make-hash-table)))
                 (loop for string in strings
                       when (< 0 (- (length (string string)) i))
                         do (push string
                                  (gethash (aref (string string) i) map)))
                 (alexandria:hash-table-alist map)))
             (build-case (i strings)
               (let ((alist (grouping i strings)))
                 (if alist
                     `(case (the character (svref +tokens+ byte))
                        (#\Nul (error 'invalid-header-token))
                        ,@(loop for (char . candidates) in alist
                                collect `(,char (advance) ,(build-case (1+ i) candidates)))
                        (otherwise (return-from ,expect-block
                                     (progn ,@(cdr (assoc 'otherwise clauses))))))
                     `(return-from ,expect-block
                        (if (= byte (char-code #\:))
                            (progn ,@(cdr (assoc (intern (car strings) :keyword) clauses)))
                            (progn ,@(cdr (assoc 'otherwise clauses)))))))))
      `(block ,expect-block
         ,(build-case 0 strings)))))


;;
;; Tokens

(declaim (type (simple-array character (128)) +tokens+))
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

(declaim (type (simple-array fixnum (128)) +unhex+))
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

(defun-insane unhex-byte (byte)
  (aref +unhex+ byte))

;;
;; Main

(defun-insane parse-method (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (with-transaction (:var start)
      (values
       (prog1 (expect-one-of
                  (:CONNECT
                   :COPY
                   :CHECKOUT
                   :DELETE
                   :GET
                   :HEAD
                   :LOCK
                   :MKCOL
                   :MKCALENDAR
                   :MKACTIVITY
                   :MOVE
                   :MERGE
                   :M-SEARCH
                   :NOTIFY
                   :OPTIONS
                   :POST
                   :PROPFIND
                   :PROPPATCH
                   :PUT
                   :PURGE
                   :PATCH
                   :REPORT
                   :SEARCH
                   :SUBSCRIBE
                   :TRACE
                   :UNLOCK
                   :UNSUBSCRIBE)
                  'invalid-method)
         (expect-byte +space+ 'invalid-method))
       p))))

(defun-insane parse-url (parser callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (with-transaction (:var start)
      (skip-while (or (<= (char-code #\!) byte (char-code #\~))
                      (<= 128 byte)))
      (callback-data :url parser callbacks data start p))
    p))

(defun-insane parse-http-version (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p))
         major minor)
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (with-transaction (:var start)
      (expect-string "HTTP/" 'expect-failed nil)
      ;; Expect the HTTP major is only once digit.
      (expect (digit-byte-char-p byte))
      (setq major (digit-byte-char-to-integer byte))
      (expect-byte (char-code #\.))
      ;; Expect the HTTP minor is only once digit.
      (expect (digit-byte-char-p byte))
      (setq minor (digit-byte-char-to-integer byte)))

    (values major minor p)))

(defun-insane parse-status-code (parser callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (unless (digit-byte-char-p byte)
      (error 'invalid-status))
    (setf (parser-status-code parser) (digit-byte-char-to-integer byte))
    (with-transaction (:var start)
      (loop
        (advance)
        (cond
          ((digit-byte-char-p byte)
           (setf (parser-status-code parser)
                 (+ (the fixnum (* 10 (parser-status-code parser)))
                    (digit-byte-char-to-integer byte)))
           (when (< 999 (parser-status-code parser))
             (error 'invalid-status :status-code (parser-status-code parser))))
          ((= byte +space+)
           ;; Reading the status text
           (skip-while (= byte +space+))
           (let ((status-text-start p))
             (skip-until-crlf)
             (callback-data :status parser callbacks data status-text-start (- p 1)))
           (advance)
           (return))
          ((= byte +cr+)
           ;; No status text
           (expect-byte +lf+)
           (advance)
           (return))
          (T (error 'invalid-status)))))
    p))

(defmacro parse-header-field-and-value ()
  `(macrolet ((skip-until-field-end ()
                `(do ((char (svref +tokens+ byte)
                            (svref +tokens+ byte)))
                     ((= byte (char-code #\:)))
                   (declare (type character char))
                   (when (char= char #\Nul)
                     (error 'invalid-header-token))
                   (advance)))
              (skip-until-value-start-and (&body body)
                `(progn
                    ;; skip #\: and leading spaces
                   (advance)
                   (skip-while-spaces)
                   (casev= byte
                     (+cr+
                      ;; continue to the next line
                      (expect-byte +lf+)
                      (advance)
                      (casev= byte
                        ((+space+ +tab+)
                         (skip-while-spaces)
                         (if (= byte +cr+)
                             ;; empty body
                             (progn
                               (expect-byte +lf+)
                               (advance)
                               (callback-data :header-field parser callbacks data field-start field-end)
                               (callback-data :header-value parser callbacks data p p))
                             (progn ,@body)))
                        (otherwise
                         ;; empty body
                         (callback-data :header-field parser callbacks data field-start field-end)
                         (callback-data :header-value parser callbacks data p p))))
                     (otherwise ,@body)))))
     (let ((field-start p) field-end)
       (declare (dynamic-extent field-start field-end))
       (case-expect-header-field ("content-length" "transfer-encoding" "upgrade")
         (:|content-length|
           (setq field-end p)
           (skip-until-value-start-and
            (multiple-value-bind (value-start value-end next content-length)
                (parse-header-value-content-length data p end)
              (declare (type pointer next))
              (setf (parser-content-length parser) content-length)
              (advance-to next)
              (callback-data :header-field parser callbacks data field-start field-end)
              (callback-data :header-value parser callbacks data value-start value-end))))
         (:|transfer-encoding|
           (setq field-end p)
           (skip-until-value-start-and
            (multiple-value-bind (value-start value-end next chunkedp)
                (parse-header-value-transfer-encoding data p end)
              (declare (type pointer next))
              (setf (parser-chunked-p parser) chunkedp)
              (advance-to next)
              (callback-data :header-field parser callbacks data field-start field-end)
              (callback-data :header-value parser callbacks data value-start value-end))))
         (:|upgrade|
           (setq field-end p)
           (setf (parser-upgrade-p parser) T)
           (skip-until-value-start-and
            (let ((value-start p))
              (skip-until-crlf)
              (advance)
              (callback-data :header-field parser callbacks data field-start field-end)
              (callback-data :header-value parser callbacks data value-start (- p 2)))))
         (otherwise (skip-until-field-end)
                    (setq field-end p)
                    (skip-until-value-start-and
                     (parse-header-value field-start field-end)))))))

(defmacro parse-header-value (&optional field-start field-end)
  `(let ((value-start p))
     (skip-until-crlf)
     (advance)
     ,@(and field-start field-end
            `((callback-data :header-field parser callbacks data ,field-start ,field-end)))
     (callback-data :header-value parser callbacks data value-start (- p 2))))

(defun-speedy parse-header-value-transfer-encoding (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))

    (if (expect-string "chunked" nil nil nil)
        (progn
          (advance)
          (casev= byte
            (+cr+ (expect-byte +lf+)
                  (values start (1- p) (1+ p) t))
            (otherwise
             (skip-until-crlf)
             (advance)
             (values start (- p 2) p nil))))
        (progn
          (skip-until-crlf)
          (advance)
          (values start (- p 2) p nil)))))

(defun-speedy parse-header-value-content-length (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p))
         (content-length 0))
    (declare (type pointer p)
             (type (unsigned-byte 8) byte)
             (type integer content-length))
    (unless (digit-byte-char-p byte)
      (error 'invalid-content-length))
    (setq content-length (digit-byte-char-to-integer byte))
    (loop
      (advance)
      (cond
        ((digit-byte-char-p byte)
         (setq content-length
               (+ (* 10 content-length)
                  (digit-byte-char-to-integer byte))))
        ((= byte +cr+)
         (expect-byte +lf+)
         (return (values start (1- p) (1+ p) content-length)))
        ((= byte +space+)
         ;; Discard spaces
         )
        (T (error 'invalid-content-length))))))

(defmacro case-header-line-start (&body clauses)
  `(casev= byte
     ((+tab+ +space+)
      ,@(cdr (assoc :value clauses)))
     (+cr+ (expect-byte +lf+)
           (incf p)
           ,@(cdr (assoc :last clauses)))
     (otherwise ,@(cdr (assoc :field clauses)))))

(defmacro parse-header-line ()
  `(case-header-line-start
    (:field (parse-header-field-and-value))
    ;; folding value
    (:value (parse-header-value))
    (:last (return))))

(defun-speedy parse-headers (parser callbacks data start end)
  (declare (type parser parser)
           (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type pointer p)
             (type (unsigned-byte 8) byte))
    ;; empty headers
    (when (= byte +cr+)
      (expect-byte +lf+)
      (return-from parse-headers (1+ p)))
    (with-transaction (:var start)
      (parse-header-field-and-value))
    (loop
      (when (= +max-header-line+ (incf (parser-header-read parser)))
        (error 'header-overflow))
      (with-transaction (:var start)
        (parse-header-line)))
    p))

(defun-speedy read-body-data (parser callbacks data start end)
  (declare (type parser parser)
           (type simple-byte-vector data)
           (type pointer start end))
  (let ((readable-count (the pointer (- end start))))
    (declare (dynamic-extent readable-count)
             (type pointer readable-count))
    (if (<= (parser-content-length parser) readable-count)
        (let ((body-end (+ start (parser-content-length parser))))
          (declare (dynamic-extent body-end))
          (setf (parser-content-length parser) 0)
          (callback-data :body parser callbacks data start body-end)
          body-end)
        ;; still needs to read
        (progn
          (decf (parser-content-length parser) readable-count)
          (callback-data :body parser callbacks data start end)
          (error 'eof :pointer end)))))

(defun-speedy http-message-needs-eof-p (parser)
  (let ((status-code (parser-status-code parser)))
    (declare (type status-code status-code))
    (when (= status-code 0) ;; probably request
      (return-from http-message-needs-eof-p nil))

    (when (and (< 99 status-code 200) ;; 1xx e.g. Continue
               (= status-code 204)    ;; No Content
               (= status-code 304)    ;; Not Modified
               )
      (return-from http-message-needs-eof-p nil))

    (when (or (parser-chunked-p parser)
              (parser-content-length parser))
      (return-from http-message-needs-eof-p nil))
    T))

(defun-speedy parse-body (parser callbacks data start end)
  (declare (type parser parser)
           (type simple-byte-vector data)
           (type pointer start end))
  (case (parser-content-length parser)
    (0
     ;; Content-Length header given but zero: Content-Length: 0\r\n
     (callback-notify :message-complete parser callbacks)
     start)
    ('nil
     (if (http-message-needs-eof-p parser)
         ;; read until EOF
         (progn
           (callback-data :body parser callbacks data start end)
           end)
         ;; message complete
         (progn
           (callback-notify :message-complete parser callbacks)
           end)))
    (otherwise
     ;; Content-Length header given and non-zero
     (read-body-data parser callbacks data start end)
     (callback-notify :message-complete parser callbacks))))

(defmacro parse-chunked-size ()
  `(let ((unhex-val (unhex-byte byte)))
     (declare (type fixnum unhex-val)
              (dynamic-extent unhex-val))
     (when (= unhex-val -1)
       (error 'invalid-chunk-size))
     (setf (parser-content-length parser) unhex-val)

     (loop
       (advance)
       (cond
         ((= byte +cr+)
          (expect-byte +lf+)
          (advance)
          (return))
         (T
          (setq unhex-val (unhex-byte byte))
          (cond
            ((= unhex-val -1)
             (case-byte byte
               ((#\; #\Space)
                ;; skipping chunk parameters
                (skip-until-crlf)
                (advance)
                (return))
               (otherwise
                (error 'invalid-chunk-size))))
            (T
             (setf (parser-content-length parser)
                   (+ (* 16 (parser-content-length parser)) unhex-val)))))))))

(defun-speedy parse-chunked-body (parser callbacks data start end)
  (declare (type parser parser)
           (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type pointer p)
             (type (unsigned-byte 8) byte))

    (with-transaction (:var start)
      (parse-chunked-size))

    (cond
      ((zerop (parser-content-length parser))
       ;; trailing
       (parse-headers parser callbacks data p end))
      (T
       (advance-to (read-body-data parser callbacks data p end))
       (expect-crlf)
       (advance)
       (parse-chunked-body parser callbacks data p end)))))

(defun-speedy parse-request (parser callbacks data &key (start 0) end)
  (declare (type parser parser)
           (type simple-byte-vector data))
  (let* ((p start)
         (byte (aref data p))
         (end (or end (length data))))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))

    (check-eof)

    ;; skip first empty line (some clients add CRLF after POST content)
    (when (= byte +cr+)
      (expect-byte +lf+)
      (advance))

    (callback-notify :message-begin parser callbacks)

    (multiple-value-bind (method next)
        (parse-method data p end)
      (declare (type pointer next))
      (setf (parser-method parser) method)
      (advance-to next))
    (skip-while (= byte +space+))
    (let ((next (parse-url parser callbacks data p end)))
      (declare (type pointer next))
      (advance-to next))

    (skip-while (= byte +space+))

    (cond
      ;; No HTTP version
      ((= byte +cr+)
       (expect-byte +lf+)
       (advance))
      (T (multiple-value-bind (major minor next)
             (parse-http-version data p end)
           (declare (type pointer next))
           (setf (parser-http-major parser) major
                 (parser-http-minor parser) minor)
           (advance-to next))

         (advance)
         (expect-crlf)
         (advance)))

    (callback-notify :first-line parser callbacks)

    (setq p (parse-headers parser callbacks data p end))

    (callback-notify :headers-complete parser callbacks)
    (setf (parser-header-read parser) 0)

    ;; Exit, the rest of the connect is in a different protocol.
    (when (parser-upgrade-p parser)
      (callback-notify :message-complete parser callbacks)
      (return-from parse-request p))

    (if (parser-chunked-p parser)
        (parse-chunked-body parser callbacks data p end)
        (parse-body parser callbacks data p end))))

(defun-speedy parse-response (parser callbacks data &key (start 0) end)
  (declare (type parser parser)
           (type simple-byte-vector data))
  (let* ((p start)
         (byte (aref data p))
         (end (or end (length data))))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))

    (multiple-value-bind (major minor next)
        (parse-http-version data p end)
      (declare (type pointer next))
      (setf (parser-http-major parser) major
            (parser-http-minor parser) minor)
      (advance-to next))

    (advance)

    (cond
      ((= byte +space+)
       (advance)
       (advance-to (parse-status-code parser callbacks data p end)))
      ((= byte +cr+)
       (expect-byte +lf+)
       (advance))
      (T (error 'invalid-version)))

    (callback-notify :first-line parser callbacks)

    (setq p (parse-headers parser callbacks data p end))
    (setf (parser-header-read parser) 0)

    (if (parser-chunked-p parser)
        (parse-chunked-body parser callbacks data p end)
        (parse-body parser callbacks data p end))))

(defun-speedy http-parse (parser callbacks data &key (start 0) end)
  (declare (type parser parser)
           (type simple-byte-vector data)
           (type pointer start))

  (case (parser-type parser)
    (:both)
    (:request
     (return-from http-parse
       (parse-request parser callbacks data :start start :end end)))
    (:response
     (return-from http-parse
       (parse-response parser callbacks data :start start :end end))))

  (let ((end (or end (length data))))
    (declare (type pointer end))
    (when (= start end)
      (error 'eof :pointer start))
    (let ((byte (aref data start)))
      (declare (type (unsigned-byte 8) byte))
      (case-byte byte
        (#\H
         (when (= (1+ start) end)
           (error 'eof :pointer start))
         (setq byte (aref data (1+ start)))
         (case-byte byte
           (#\T (setf (parser-type parser) :response)
                (parse-response parser callbacks data :start start :end end))
           (#\E (setf (parser-type parser) :request)
                (parse-request parser callbacks data :start start :end end))
           (otherwise
            (error 'invalid-constant))))
        (otherwise
         (setf (parser-type parser) :request)
         (parse-request parser callbacks data :start start :end end))))))
