(in-package :cl-user)
(defpackage fast-http.byte-vector
  (:use :cl)
  (:import-from :alexandria
                :define-constant)
  (:export :+cr+
           :+lf+
           :+space+
           :+tab+
           :+page+
           :+crlf+

           :simple-byte-vector
           :make-byte-vector
           :digit-byte-char-p
           :digit-byte-char-to-integer
           :alpha-byte-char-p
           :alpha-byte-char-to-lower-char
           :alphanumeric-byte-char-p
           :mark-byte-char-p
           :ascii-octets-to-upper-string
           :case-byte))
(in-package :fast-http.byte-vector)

(defconstant +cr+ (char-code #\Return))
(defconstant +lf+ (char-code #\Newline))
(defconstant +space+ (char-code #\Space))
(defconstant +tab+ (char-code #\Tab))
(defconstant +page+ (char-code #\Page))

(define-constant +crlf+
  (make-array 2 :element-type '(unsigned-byte 8)
                :initial-contents (list +cr+ +lf+))
  :test 'equalp)

(deftype simple-byte-vector (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))

(declaim (inline digit-byte-char-p
                 digit-byte-char-to-integer
                 alpha-byte-char-p
                 alpha-byte-char-to-lower-char
                 alphanumeric-byte-char-p
                 mark-byte-char-p))

(defun digit-byte-char-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (<= #.(char-code #\0) byte #.(char-code #\9)))

(declaim (ftype (function ((unsigned-byte 8)) fixnum) digit-byte-char-to-integer))
(defun digit-byte-char-to-integer (byte)
  (declare (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (the fixnum (- byte #.(char-code #\0))))

(defun alpha-byte-char-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (or (<= #.(char-code #\A) byte #.(char-code #\Z))
      (<= #.(char-code #\a) byte #.(char-code #\z))))

(defun alpha-byte-char-to-lower-char (byte)
  (declare (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (the character
       (cond
         ((<= #.(char-code #\A) byte #.(char-code #\Z))
          (code-char (+ byte #x20)))
         (T #+nil(<= #.(char-code #\a) byte #.(char-code #\z))
            (code-char byte)))))

(defun alphanumeric-byte-char-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (or (alpha-byte-char-p byte)
      (digit-byte-char-p byte)))

(defun mark-byte-char-p (byte)
  (declare (type (unsigned-byte 8) byte))
  (or (= byte #.(char-code #\-))
      (= byte #.(char-code #\_))
      (= byte #.(char-code #\.))
      (= byte #.(char-code #\!))
      (= byte #.(char-code #\~))
      (= byte #.(char-code #\*))
      (= byte #.(char-code #\'))
      (= byte #.(char-code #\())
      (= byte #.(char-code #\)))))

(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) byte-to-ascii-upper)
         (inline byte-to-ascii-upper))
(defun byte-to-ascii-upper (x)
  (declare (type (unsigned-byte 8) x)
           (optimize (speed 3) (safety 0)))
  (if (>= #.(char-code #\z) x #.(char-code #\a))
      (+ #.(- (char-code #\A) (char-code #\a)) x)
      x))

(defun ascii-octets-to-upper-string (octets &key (start 0) end)
  (declare (type simple-byte-vector octets)
           (type (unsigned-byte 64) start)
           (optimize (speed 3) (safety 0)))
  (let* ((end (or end (length octets)))
         (len (- end start))
         (string (make-string len :element-type 'character)))
    (declare (type (unsigned-byte 64) end))
    (do ((i 0 (1+ i))
         (j start (1+ j)))
        ((= j end) string)
      (setf (aref string i)
            (code-char (byte-to-ascii-upper (aref octets j)))))))

(defmacro case-byte (byte &body cases)
  `(case ,byte
     ,@(loop for (val . form) in cases
             if (eq val 'otherwise)
               collect `(,val ,@form)
             else if (listp val)
               collect `(,(mapcar #'char-code val) ,@form)
             else
               collect `(,(char-code val) ,@form))))
