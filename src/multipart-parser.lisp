(in-package :cl-user)
(defpackage fast-http.multipart-parser
  (:use :cl
        :fast-http.byte-vector
        :fast-http.error)
  (:import-from :fast-http.parser
                :callbacks-body
                :callbacks-headers-complete
                :callbacks-message-begin
                :callbacks-message-complete
                :make-callbacks
                :parse-headers
                :eof)
  (:import-from :fast-http.http
                :http-state
                :http-mark
                :make-http
                :+state-headers+)
  (:import-from :fast-http.util
                :tagcasev
                :casev)
  (:import-from :alexandria
                :format-symbol
                :when-let)
  (:export :ll-multipart-parser
           :ll-multipart-parser-state
           :make-ll-multipart-parser
           :http-multipart-parse))
(in-package :fast-http.multipart-parser)

(defstruct (ll-multipart-parser (:constructor make-ll-multipart-parser
                                  (&key boundary
                                   &aux (header-parser
                                         (let ((parser (make-http)))
                                           (setf (http-state parser) +state-headers+)
                                           parser)))))
  (state 0 :type fixnum)
  (header-parser)
  boundary
  ;; Start of the part body not sent to the body callback yet.
  ;; NIL while no part is open (before the first delimiter, after the last).
  body-mark
  ;; Position of the CR that may start a delimiter (\r\n--boundary).
  boundary-mark
  ;; Bytes at the end of the previous call that could not be decided yet
  ;; (an incomplete delimiter or incomplete part headers). They are parsed
  ;; again together with the data of the next call.
  pending)

(declaim (type fixnum +max-header-buffer-bytes+))
(defconstant +max-header-buffer-bytes+ 8192
  "Maximum number of bytes of an incomplete part header line that is
kept between calls of HTTP-MULTIPART-PARSE.

Without this limit an unterminated header line that keeps arriving in
small chunks would be buffered without bound, and every call would copy
and re-parse the whole buffer (O(N^2)).")

#.`(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for i from 0
             for state in '(parsing-delimiter-dash-start
                            parsing-delimiter-dash
                            parsing-delimiter
                            parsing-delimiter-end
                            parsing-delimiter-almost-done
                            parsing-delimiter-done
                            header-field-start
                            body-start
                            looking-for-delimiter
                            maybe-delimiter-start
                            maybe-delimiter-first-dash
                            maybe-delimiter-second-dash
                            body-almost-done
                            body-done)
             collect `(defconstant ,(format-symbol t "+~A+" state) ,i)))

(defun http-multipart-parse (parser callbacks data &key (start 0) end)
  (declare (type simple-byte-vector data))
  (let* ((end (or end (length data)))
         (boundary (map '(simple-array (unsigned-byte 8) (*)) #'char-code (ll-multipart-parser-boundary parser)))
         (boundary-length (length boundary))
         (header-parser (ll-multipart-parser-header-parser parser)))
    (declare (type simple-byte-vector boundary))
    (when (= start end)
      (return-from http-multipart-parse start))
    ;; Parse the bytes kept at the end of the previous call again, together
    ;; with the new data, so that a delimiter or part headers split across
    ;; calls are seen as contiguous bytes. ORIG-START and PREPENDED record how
    ;; the coordinates shifted, so the position returned to the caller stays
    ;; relative to the data of THIS call (see the end of the function).
    (let ((orig-start start)
          (prepended 0))
     (when-let (pending (ll-multipart-parser-pending parser))
      (setf data (concatenate 'simple-byte-vector pending (subseq data start end))
            start 0
            end (length data)
            prepended (length pending)
            (ll-multipart-parser-pending parser) nil))

    (macrolet ((call-body-cb (&optional (end '(ll-multipart-parser-boundary-mark parser)))
                 ;; Send DATA[BODY-MARK, END) to the body callback.
                 (let ((g-start (gensym "START"))
                       (g-end (gensym "END")))
                   `(let ((,g-start (ll-multipart-parser-body-mark parser))
                          (,g-end ,end))
                      (when (< ,g-start ,g-end)
                        (handler-case (when-let (callback (callbacks-body callbacks))
                                        (funcall callback parser data ,g-start ,g-end))
                          (error (e)
                            (error 'cb-body :error e))))))))
      (let* ((p start)
             (byte (aref data p)))
        #+fast-http-debug
        (log:debug (code-char byte))
        (tagbody
           (macrolet ((go-state (tag &optional (advance 1))
                          `(progn
                             ,(case advance
                                (0 ())
                                (1 '(incf p))
                                (otherwise `(incf p ,advance)))
                             (setf (ll-multipart-parser-state parser) ,tag)
                             #+fast-http-debug
                             (log:debug ,(princ-to-string tag))
                             ,@(and (not (eql advance 0))
                                    `((when (= p end)
                                        (go exit-loop))
                                      (setq byte (aref data p))
                                      #+fast-http-debug
                                      (log:debug (code-char byte))))
                             (go ,tag))))
             (tagcasev (ll-multipart-parser-state parser)
               (+parsing-delimiter-dash-start+
                (unless (= byte +dash+)
                  (go-state +header-field-start+ 0))
                (go-state +parsing-delimiter-dash+))

               (+parsing-delimiter-dash+
                (unless (= byte +dash+)
                  (error 'invalid-multipart-body))
                (go-state +parsing-delimiter+))

               (+parsing-delimiter+
                (let ((end2 (+ p boundary-length)))
                  (when (< end end2)
                    ;; The rest of the boundary is in the next call.
                    (go exit-loop))
                  (when (mismatch boundary data :start2 p :end2 end2)
                    ;; Still in the body. Re-examine this byte: it may be
                    ;; the CR of the real delimiter.
                    (when (ll-multipart-parser-body-mark parser)
                      (go-state +looking-for-delimiter+ 0))
                    (error 'invalid-boundary))
                  (go-state +parsing-delimiter-end+ boundary-length)))

               (+parsing-delimiter-end+
                (casev byte
                  (+cr+ (go-state +parsing-delimiter-almost-done+))
                  (+lf+ (go-state +parsing-delimiter-almost-done+ 0))
                  (+dash+ (go-state +body-almost-done+))
                  (otherwise
                   ;; Still in the body
                   (when (ll-multipart-parser-body-mark parser)
                     (go-state +looking-for-delimiter+ 0))
                   (error 'invalid-boundary))))

               (+parsing-delimiter-almost-done+
                (unless (= byte +lf+)
                  (error 'invalid-boundary))
                (when (ll-multipart-parser-body-mark parser)
                  ;; got a part
                  (call-body-cb)
                  (when-let (callback (callbacks-message-complete callbacks))
                    (handler-case (funcall callback parser)
                      (error (e)
                        (error 'cb-message-complete :error e)))))
                (go-state +parsing-delimiter-done+))

               (+parsing-delimiter-done+
                (when-let (callback (callbacks-message-begin callbacks))
                  (handler-case (funcall callback parser)
                    (error (e)
                      (error 'cb-message-begin :error e))))
                (setf (ll-multipart-parser-body-mark parser) p)
                (go-state +header-field-start+ 0))

               (+header-field-start+
                ;; P is the first byte of this part's header block, and bytes
                ;; kept from an earlier call were prepended at the top of this
                ;; function, so [P, END) holds every header byte received for
                ;; this part so far.
                ;;
                ;; First probe the block with a throwaway parser that runs no
                ;; callbacks. If the headers are incomplete, keep the whole
                ;; block and probe it again next call; only once PARSE-HEADERS
                ;; consumes the block without signalling EOF is it parsed for
                ;; real. Re-parsing the whole block each time, rather than
                ;; resuming from the last complete line, keeps the result
                ;; independent of how the input is split -- including a header
                ;; value folded onto a later line (obs-fold), whose
                ;; continuation cannot be resumed from its own start.
                ;; EOF is not an ERROR, so it must never escape to the caller.
                (handler-case
                    (let ((probe (make-http)))
                      (setf (http-state probe) +state-headers+)
                      (parse-headers probe (make-callbacks) data p end))
                  (eof ()
                    (let ((rest (subseq data p end)))
                      (when (< +max-header-buffer-bytes+ (length rest))
                        (error 'header-overflow))
                      (setf (ll-multipart-parser-pending parser) rest))
                    (go exit-loop)))
                ;; The headers are complete in [P, END): parse them once for
                ;; real. PARSE-HEADERS only returns at the end of the headers.
                (setf (http-mark header-parser) p
                      (http-state header-parser) +state-headers+)
                (let ((next (parse-headers header-parser callbacks data p end)))
                  (setq p (1- next)) ;; XXX
                  (when-let (callback (callbacks-headers-complete callbacks))
                    (handler-case (funcall callback parser)
                      (error (e)
                        (error 'cb-headers-complete :error e))))
                  (setf (http-state header-parser) +state-headers+)
                  (go-state +body-start+ 0)))

               (+body-start+
                (setf (ll-multipart-parser-body-mark parser) (1+ p)
                      (ll-multipart-parser-boundary-mark parser) nil)
                (go-state +looking-for-delimiter+))

               (+looking-for-delimiter+
                (setf (ll-multipart-parser-boundary-mark parser) nil)
                (casev byte
                  (+cr+ (setf (ll-multipart-parser-boundary-mark parser) p)
                        (go-state +maybe-delimiter-start+))
                  (otherwise (go-state +looking-for-delimiter+))))

               ;; In the following states, a byte that doesn't continue the
               ;; delimiter is re-examined by +LOOKING-FOR-DELIMITER+ (advance 0)
               ;; because it may be the CR of the real delimiter.
               (+maybe-delimiter-start+
                (unless (= byte +lf+)
                  (go-state +looking-for-delimiter+ 0))
                (go-state +maybe-delimiter-first-dash+))

               (+maybe-delimiter-first-dash+
                (unless (= byte +dash+)
                  (go-state +looking-for-delimiter+ 0))
                (go-state +maybe-delimiter-second-dash+))

               (+maybe-delimiter-second-dash+
                (unless (= byte +dash+)
                  (go-state +looking-for-delimiter+ 0))
                (go-state +parsing-delimiter+))

               (+body-almost-done+
                (casev byte
                  (+dash+ (go-state +body-done+ 0))
                  (otherwise (error 'invalid-multipart-body))))

               (+body-done+
                (when (ll-multipart-parser-body-mark parser)
                  ;; got a part
                  (call-body-cb)
                  (when-let (callback (callbacks-message-complete callbacks))
                    (handler-case (funcall callback parser)
                      (error (e)
                        (error 'cb-message-complete :error e))))
                  (setf (ll-multipart-parser-body-mark parser) nil))
                (go exit-loop))))
         exit-loop)
        (let ((state (ll-multipart-parser-state parser)))
          (declare (type fixnum state))
          (cond
            ((null (ll-multipart-parser-body-mark parser))
             ;; The first delimiter is incomplete: parse it again from the
             ;; beginning on the next call.
             (when (or (<= +parsing-delimiter-dash+ state +parsing-delimiter-almost-done+)
                       (= state +body-almost-done+))
               (setf (ll-multipart-parser-pending parser) (subseq data start end)
                     (ll-multipart-parser-state parser) +parsing-delimiter-dash-start+)))
            ((= state +looking-for-delimiter+)
             ;; In the middle of a part body.
             (call-body-cb p))
            ((or (<= +parsing-delimiter+ state +parsing-delimiter-almost-done+)
                 (<= +maybe-delimiter-start+ state +body-almost-done+))
             ;; A delimiter may start at BOUNDARY-MARK but is incomplete.
             ;; Send the body before it and parse the rest again on the next call.
             (call-body-cb)
             (setf (ll-multipart-parser-pending parser)
                   (subseq data (ll-multipart-parser-boundary-mark parser) end)
                   (ll-multipart-parser-state parser) +looking-for-delimiter+)))
          ;; Positions are relative to DATA, which is gone on the next call.
          (when (ll-multipart-parser-body-mark parser)
            (setf (ll-multipart-parser-body-mark parser) 0))
          (setf (ll-multipart-parser-boundary-mark parser) nil))
        ;; P indexes DATA, which may begin with bytes re-parsed from the
        ;; previous call. Return a position relative to the data passed in
        ;; THIS call, so a caller advancing its own cursor is never sent past
        ;; the data it gave (a position inside the re-parsed bytes means none
        ;; of this call's data was consumed, so return its start).
        (if (zerop prepended)
            p
            (max orig-start (+ orig-start (- p prepended)))))))))
