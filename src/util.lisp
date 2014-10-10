(in-package :cl-user)
(defpackage fast-http.util
  (:use :cl)
  (:import-from :fast-http.error
                :strict-error)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :alexandria
                :once-only
                :ensure-list)
  (:import-from :cl-utilities
                :with-collectors)
  (:export :check-strictly
           :casev
           :tagcase
           :tagcasev
           :make-collector
           :number-string-p))
(in-package :fast-http.util)

(defmacro check-strictly (form)
  `(unless ,form
     (error 'strict-error :form ',form)))

(defmacro casev (keyform &body clauses)
  (once-only (keyform)
    (flet ((get-val (val)
             (cond
               ((eq val 'otherwise) val)
               ((symbolp val) (symbol-value val))
               ((constantp val) val)
               (T (error "CASEV can be used only with variables or constants")))))
      `(case ,keyform
         ,@(loop for (val . clause) in clauses
                 if (eq val 'otherwise)
                   collect `(otherwise ,@clause)
                 else if (listp val)
                   collect `((,@(mapcar #'get-val val)) ,@clause)
                 else
                   collect `(,(get-val val) ,@clause))))))

(defmacro tagcase (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (case ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                    collect `(otherwise ,@body (go ,end))
                  else
                    collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                  append tag
                else
                  collect tag
                collect `(progn ,@body
                                (go ,end)))
      ,end)))

(defmacro tagcasev (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (casev ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                    collect `(otherwise ,@body (go ,end))
                  else
                    collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                  append tag
                else if (not (eq tag 'otherwise))
                       collect tag
                collect `(progn ,@body
                                (go ,end)))
      ,end)))

(defun make-collector ()
  (let ((none '#:none))
    (declare (dynamic-extent none))
    (with-collectors (buffer)
      (return-from make-collector
        (lambda (&optional (data none))
          (unless (eq data none)
            (buffer data))
          buffer)))))

(defun number-string-p (string)
  (declare (type string string))
  (not (null (ppcre:scan "^(?:\\d*[.])?\\d+$" string))))
