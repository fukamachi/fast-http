(in-package :cl-user)
(defpackage fast-http.subseqs
  (:use :cl)
  (:import-from :fast-http.byte-vector
                :simple-byte-vector)
  (:import-from :babel
                :*suppress-character-coding-errors*
                :*default-character-encoding*
                :*string-vector-mappings*
                :lookup-mapping
                :code-point-counter
                :decoder)
  (:export :octets-to-string-into-string
           :make-byte-vector-subseq
           :byte-vector-subseqs-to-string))
(in-package :fast-http.subseqs)

(defun octets-to-string-into-string (string vector &key (start1 0) (start2 0) end2
                                                     (encoding babel:*default-character-encoding*))
  (declare (type string string)
           (type simple-byte-vector vector)
           (optimize (speed 3) (safety 0)))
  (babel::with-checked-simple-vector ((vector vector) (start start2) (end end2))
    (let ((mapping (babel::lookup-mapping babel::*string-vector-mappings* encoding)))
      (multiple-value-bind (size new-end2)
          (funcall (babel::code-point-counter mapping) vector start2 (or end2 (length vector)) -1)
        (funcall (babel::decoder mapping) vector start2 new-end2 string start1)
        (values string (+ start1 size))))))

(defstruct (byte-vector-subseq (:constructor make-byte-vector-subseq (data start end)))
  (data nil :type simple-byte-vector)
  (start 0 :type integer)
  (end 0 :type integer))

(defun byte-vector-subseqs-to-string (subseqs length &key (encoding *default-character-encoding*))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((string (make-string length :element-type 'babel::unicode-char))
         (current-pos 0))
    (loop for subseq in subseqs
          do (setq current-pos
                   (nth-value 1
                              (octets-to-string-into-string (the string string)
                                                            (the simple-byte-vector
                                                                 (byte-vector-subseq-data subseq))
                                                            :start1 current-pos
                                                            :start2 (byte-vector-subseq-start subseq)
                                                            :end2 (byte-vector-subseq-end subseq)
                                                            :encoding encoding))))
    string))
