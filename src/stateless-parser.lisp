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
           :parser-method
           :parser-http-major
           :parser-http-minor
           :parser-content-length
           :parser-chunked-p
           :parser-upgrade-p
           :parse-request))
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
;; Types

(deftype pointer () 'integer)


;; Parser declaration

(defstruct parser
  (method nil :type symbol)
  (http-major 0 :type fixnum)
  (http-minor 0 :type fixnum)
  (content-length nil :type (or null integer))
  (chunked-p nil :type boolean)
  (upgrade-p nil :type boolean))


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

(defmacro callback-data (name parser data start end)
  (with-gensyms (callback)
    `(when-let (,callback (,(intern (format nil "~A-~A" :callbacks name)) callbacks))
       (funcall ,callback ,parser ,data ,start ,end))))


;;
;; Parser utilities

(define-condition eof ()
  ((pointer :initarg :pointer
            :initform nil
            :reader eof-pointer)))

(defmacro check-eof ()
  `(when (= p end)
     (error 'eof)))

(defmacro with-transaction (&body body)
  (with-gensyms (start e)
    `(let ((,start p))
       (handler-case (progn ,@body)
         (eof (,e)
           (if (eof-pointer ,e)
               (error ,e)
               (error 'eof :pointer ,start)))))))

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
  `(loop do (advance)
         while (progn ,form)))

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
     (unless (progn ,form)
       (error ,error))))

(defmacro expect-byte (byte &optional (error ''expect-failed) (advance T))
  "Advance the pointer and check if the next byte is BYTE."
  `(expect (= byte ,byte) ,error ,advance))

(defmacro expect-char (char &optional (error ''expect-failed) (advance T))
  `(expect-byte ,(char-code char) ,error ,advance))

(defmacro expect-string (string &optional (error ''expect-failed) (advance T))
  (if advance
      `(progn
         ,@(loop for char across string
                 collect `(expect-char ,char ,error)))
      (when (/= 0 (length string))
        `(progn
           (expect-char ,(aref string 0) ,error nil)
           (expect-string ,(subseq string 1) ,error)))))

(defmacro expect-crlf (&optional (error ''expect-failed))
  `(casev= byte
     (+cr+ (expect-byte +lf+ ,error))
     (otherwise (error ,error))))

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
                                        (expect-string ,(subseq (string (car candidates)) (1+ i)) ,error)
                                        (return-from ,expect-block ,(car candidates))))
                    (otherwise ,(if error
                                    `(error ,error)
                                    `(return-from ,expect-block nil)))))))
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


;;
;; Main

(defun-insane parse-method (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (values (prog1 (expect-one-of
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
            p)))

(defun-insane parse-url (parser callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (skip-while (or (<= (char-code #\!) byte (char-code #\~))
                    (<= 128 byte)))
    (callback-data :url parser data start p)
    p))

(defun-insane parse-http-version (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p))
         major minor)
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (expect-string "HTTP/" 'expect-failed nil)
    ;; Expect the HTTP major is only once digit.
    (expect (digit-byte-char-p byte))
    (setq major (digit-byte-char-to-integer byte))
    (expect-byte (char-code #\.))
    ;; Expect the HTTP minor is only once digit.
    (expect (digit-byte-char-p byte))
    (setq minor (digit-byte-char-to-integer byte))

    (values major minor p)))

(defmacro parse-header-field-and-value ()
  `(macrolet ((skip-until-field-end ()
                `(do ((char (svref +tokens+ byte)
                            (svref +tokens+ byte)))
                     ((= byte (char-code #\:)))
                   (declare (type character char))
                   (when (char= char #\Nul)
                     (error 'invalid-header-token))
                   (advance))))
     (let ((field-start p) field-end)
       (declare (dynamic-extent field-start field-end))
       (case-expect-header-field ("content-length" "transfer-encoding" "upgrade")
         (:|content-length|
           (setq field-end p)
           ;; skip #\: and leading spaces
           (advance) (skip-while-spaces)
           (multiple-value-bind (value-start value-end next content-length)
               (parse-header-value-content-length data p end)
             (declare (type pointer next))
             (setf (parser-content-length parser) content-length)
             (advance-to next)
             (callback-data :header-field parser data field-start field-end)
             (callback-data :header-value parser data value-start value-end)))
         (:|transfer-encoding|
           (setq field-end p)
           ;; skip #\: and leading spaces
           (advance) (skip-while-spaces)
           (multiple-value-bind (value-start value-end next chunkedp)
               (parse-header-value-transfer-encoding data p end)
             (declare (type pointer next))
             (setf (parser-chunked-p parser) chunkedp)
             (advance-to next)
             (callback-data :header-field parser data field-start field-end)
             (callback-data :header-value parser data value-start value-end)))
         (:|upgrade|
           (setq field-end p)
           ;; skip #\: and leading spaces
           (advance) (skip-while-spaces)
           (setf (parser-upgrade-p parser) T)
           (skip-while-spaces)
           (let ((value-start p))
             (skip-until-crlf)
             (advance)
             (callback-data :header-field parser data field-start field-end)
             (callback-data :header-value parser data value-start (- p 2))))
         (otherwise (skip-until-field-end)
                    (setq field-end p)
                    (parse-header-value field-start field-end))))))

(defmacro parse-header-value (&optional field-start field-end)
  `(progn
     (skip-while-spaces)
     (let ((value-start p))
       (skip-until-crlf)
       (advance)
       ,@(and field-start field-end
              `((callback-data :header-field parser data ,field-start ,field-end)))
       (callback-data :header-value parser data value-start (- p 2)))))

(defun-speedy parse-header-value-transfer-encoding (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (if (expect-one-of ("chunked") nil nil)
        (casev= byte
          (+cr+ (expect-byte +lf+)
                (values start (1- p) (1+ p) t))
          (otherwise
           (skip-until-crlf)
           (advance)
           (values start (- p 2) p nil)))
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
      (when (= byte +cr+)
        (expect-byte +lf+)
        (return (values start (1- p) (1+ p) content-length)))
      (unless (digit-byte-char-to-integer byte)
        (error 'invalid-content-length))
      (setq content-length
            (+ (* 10 content-length)
               (digit-byte-char-to-integer byte))))))

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

;; TODO: check max header length
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
    (with-transaction
      (parse-header-field-and-value))
    (loop (with-transaction (parse-header-line)))
    p))

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
    (multiple-value-bind (major minor next)
        (parse-http-version data p end)
      (declare (type pointer next))
      (setf (parser-http-major parser) major
            (parser-http-minor parser) minor)
      (advance-to next))

    (advance)
    (expect-crlf)
    (advance)

    (parse-headers parser callbacks data p end)))
