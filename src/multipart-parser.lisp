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

(defconstant +max-boundary-length+ 70
  "Maximum length of a multipart boundary (RFC 2046). The client chooses the
boundary, and an incomplete delimiter is copied and scanned again on every
call of HTTP-MULTIPART-PARSE, so its length has to be bounded.")

(defun check-boundary (boundary)
  "Return BOUNDARY, or signal INVALID-BOUNDARY unless it is 1 to
+MAX-BOUNDARY-LENGTH+ characters long."
  (unless (<= 1 (length boundary) +max-boundary-length+)
    (error 'invalid-boundary))
  boundary)

(defun make-part-header-parser ()
  "An HTTP object ready to parse one part's header block."
  (let ((parser (make-http)))
    (setf (http-state parser) +state-headers+)
    parser))

(defstruct (ll-multipart-parser (:constructor make-ll-multipart-parser
                                  (&key ((:boundary given-boundary))
                                   &aux (boundary (check-boundary given-boundary))
                                        (header-parser (make-part-header-parser)))))
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

(declaim (type fixnum +max-part-header-bytes+))
(defconstant +max-part-header-bytes+ 8192
  "Maximum size in bytes of one part's header block, from its first byte
through the blank line that ends it. A larger block signals HEADER-OVERFLOW.

Incomplete part headers are kept between calls of HTTP-MULTIPART-PARSE until
the blank line arrives. Without this limit headers that keep arriving in small
chunks would be buffered without bound, and every call would copy the whole
buffer.

The limit applies to the whole block whether it arrives in one call or is
split across calls, so whether a request is accepted does not depend on
where the input is split.")

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

(defun join-kept-bytes (kept data start end)
  "A fresh vector of KEPT followed by DATA[START, END).
REPLACE on declared vectors copies in bulk; CONCATENATE here goes element by
element, which is the main cost when input arrives in small chunks."
  (declare (type simple-byte-vector kept data)
           (type fixnum start end))
  (let ((joined (make-array (+ (length kept) (- end start))
                            :element-type '(unsigned-byte 8))))
    (replace joined kept)
    (replace joined data :start1 (length kept) :start2 start :end2 end)
    joined))

(defun part-header-block-end (data p end search-start)
  "Position just after the blank line ending the part header block that starts
at P, or NIL if it has not arrived yet. Looks for CRLF CRLF from SEARCH-START,
which lets a caller skip bytes it has already looked at.

Only CRLF ends a header line in PARSE-HEADERS, so LF LF does not end the block.
A block that begins with CRLF is a part without headers."
  (declare (type simple-byte-vector data)
           (type fixnum p end search-start))
  (flet ((crlf-at-p (i)
           (and (< (1+ i) end)
                (= (aref data i) +cr+)
                (= (aref data (1+ i)) +lf+))))
    (if (crlf-at-p p)
        (+ p 2)
        (loop for i of-type fixnum from search-start below (- end 3)
              when (and (crlf-at-p i) (crlf-at-p (+ i 2)))
                return (+ i 4)))))

(defun http-multipart-parse (parser callbacks data &key (start 0) end)
  "Parse DATA[START, END) as the next piece of a multipart body.

Returns a position in DATA, relative to the DATA given to this call. Bytes that
cannot be decided yet (incomplete part headers or an incomplete delimiter) are
kept by PARSER and count as consumed: when any are kept, the return value is
END, and the caller must not pass them again."
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
    ;; calls are seen as contiguous bytes. ORIG-START, ORIG-END and PREPENDED
    ;; record how the coordinates shifted, so the position returned to the
    ;; caller stays relative to the data of THIS call (see the end of the
    ;; function).
    (let ((orig-start start)
          (orig-end end)
          (prepended 0))
     (when-let (pending (ll-multipart-parser-pending parser))
      (setf data (join-kept-bytes pending data start end)
            start 0
            end (length data)
            prepended (length pending)
            (ll-multipart-parser-pending parser) nil))
     ;; An open part's body continues from the first byte of this call.
     (when (ll-multipart-parser-body-mark parser)
       (setf (ll-multipart-parser-body-mark parser) start))

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
                ;; Keep the block until the blank line that ends it has arrived,
                ;; and only then parse it. Bytes before the last 3 kept ones have
                ;; already been searched, so each call does work proportional to
                ;; its own data. Parsing the whole block at once, rather than
                ;; resuming from the last complete line, keeps the result
                ;; independent of how the input is split -- including a header
                ;; value folded onto a later line (obs-fold), whose continuation
                ;; cannot be resumed from its own start.
                ;;
                ;; +MAX-PART-HEADER-BYTES+ bounds the whole block and is checked
                ;; before any parsing, the same way when it is complete and when
                ;; it is kept for the next call. Every byte kept is part of the
                ;; block, so a kept prefix exceeds the limit only if the whole
                ;; block would; neither the verdict nor the error depends on how
                ;; the input is split.
                (let ((block-end (part-header-block-end
                                  data p end (max p (- prepended 3)))))
                  (when (< +max-part-header-bytes+ (- (or block-end end) p))
                    (error 'header-overflow))
                  (unless block-end
                    (setf (ll-multipart-parser-pending parser)
                          (subseq data p end))
                    (go exit-loop)))
                ;; The blank line is in [P, END), so parse the block once.
                ;; PARSE-HEADERS only returns at the end of the headers. EOF is
                ;; not an ERROR and must never escape to the caller; with the
                ;; blank line present it cannot occur, so if it ever does the
                ;; body is rejected.
                ;; A new parser for each part: what PARSE-HEADERS records
                ;; about a block (lines read, Content-Length, Transfer-Encoding)
                ;; belongs to that part, and would reject the next part's
                ;; headers as duplicates or as too many lines.
                (setf header-parser (make-part-header-parser)
                      (ll-multipart-parser-header-parser parser) header-parser
                      (http-mark header-parser) p)
                (let ((next (handler-case
                                (parse-headers header-parser callbacks data p end)
                              (eof ()
                                (error 'invalid-multipart-body)))))
                  (setq p (1- next)) ;; XXX
                  (when-let (callback (callbacks-headers-complete callbacks))
                    (handler-case (funcall callback parser)
                      (error (e)
                        (error 'cb-headers-complete :error e))))
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
        ;; Bytes kept for the next call are consumed, so return the end of
        ;; this call's data. Otherwise P indexes DATA; if DATA was joined with
        ;; bytes re-parsed from the previous call, translate it to a position
        ;; in the data passed in THIS call.
        (cond
          ((ll-multipart-parser-pending parser) orig-end)
          ((zerop prepended) p)
          (t (+ orig-start (- p prepended)))))))))
