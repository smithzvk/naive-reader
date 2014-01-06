
;; Naive reader is a mock-up of the Common Lisp reader that attempts to return
;; the string that corresponds to the form that would be read from a stream.

;; This is tricky because the Common Lisp reader is so customizable.  For
;; instance, one very common reader macro is the conditional read.  With this
;; read macro, the act of reading on a stream is not really defined with out the
;; value of <*features*>, which is implementation dependent.  This means that if
;; you want a read function that reads something like forms that is able to grab
;; code that is written for a different implementation, then those forms can't
;; actually be read into the lisp system (as there may be errors in the code
;; that should not be read via lisp) so we need to only return strings.
;; Further, because we are limited to strings, we cannot actually use the reader
;; macros themselves, which means that we are actually fighting a losing battle.
;; I suspect that there is no way to actually do this correctly, but it can
;; certainly be done in a way that matches the vast majority of cases and that
;; can be extended to handle other cases.

;; This is useful for two of my current projects, my literate programming system
;; and my hacker rank submission project.

;; We define our own half assed reader that should work most of the time.

;; We need to use this rather than <read> because we want to read the source but
;; leave it intact, i.e. we don't want reader macros processed.  I wish there
;; was a better way to do this.  An example of something that this doesn't work
;; with is Clommand.

(in-package :naive-reader)

(defparameter *custom-delimiters* '((#\{ #\})))

(defvar *form-found* nil)

(define-condition extra-closing-paren (serious-condition) ())

(defmacro ignore-extra-closing-paren (&body body)
  `(if *form-found*
       (handler-case (progn ,@body)
         (extra-closing-paren ()
           '(nil)))
       (handler-case (progn ,@body)
         (end-of-file ()
           '(nil)))))

(defun read-form-string (stream &optional (eof-error-p t) eof-value)
  (let ((*form-found* nil)
        (stream (if (stringp stream)
                    (make-string-input-stream stream)
                    stream)))
    (handler-case (values
                   (apply
                    'concatenate 'string
                    (naive-read stream))
                   *form-found*)
      (end-of-file (c)
        (if eof-error-p
            eof-value
            (error c))))))

(defun slurp-whitespace (stream)
  (list
   (with-output-to-string (out)
     (block nil
       (iter (for char :in-stream-without-close stream :using #'read-char)
         (cond ((member char '(#\Space #\Tab #\Newline #\Return))
                (write-char char out))
               (t (unread-char char stream)
                  (return-from nil))))))))

(defun naive-read (stream)
  (append
   (slurp-whitespace stream)
   (let ((char (peek-char nil stream)))
     (cond ((member char '(#\' #\` #\, #\.))
            (setf *form-found* t)
            (list* (list (read-char stream))
                   (naive-read stream)))
           ((eql char #\()
            (setf *form-found* t)
            (naive-read-list stream))
           ((member char '(#\" #\|))
            (setf *form-found* t)
            (list*
             (list (read-char stream))
             (read-delimited (list char) stream)))
           ((eql char #\#)
            (dispatch-reader stream))
           ((eql char #\;)
            (append
             (read-delimited '(#\Newline #\Return) stream :allows-escaping nil)
             (ignore-extra-closing-paren (naive-read stream))))
           ((eql char #\))
            (error 'extra-closing-paren "Extra closing parenthesis"))
           ((and (not *form-found*) (member char *custom-delimiters* :key 'first))
            (naive-read-gen-list
             stream
             (second (first (member char *custom-delimiters* :key 'first)))))
           (t
            (setf *form-found* t)
            (naive-read-atom stream))))))

(defun naive-read-atom (stream)
  (list
   (let ((string
           (with-output-to-string (out)
             (block nil
               (let ((escape nil))
                 (iter (for char :in-stream-without-close stream :using #'read-char)
                   (cond ((not escape)
                          (cond ((eql #\\ char)
                                 (write-char char out)
                                 (setf escape t))
                                ((member char
                                         '(#\# #\( #\) #\` #\, #\; #\'
                                           #\Space #\Tab #\Newline #\Return))
                                 (unread-char char stream)
                                 (return-from nil))
                                (t
                                 (write-char char out))))
                         (t (write-char char out)
                            (setf escape nil)))))))))
     (if (= 0 (length string))
         (error "expected an atom, but found a ~S" (peek-char nil stream))
         string))))

(defun read-delimited (delimiter stream &key (allows-escaping t))
  (list
   (with-output-to-string (out)
     (block nil
       (let ((escaped nil))
         (iter (for char :in-stream-without-close stream :using #'read-char)
           (write-char char out)
           (if (or (not allows-escaping) (not escaped))
               (cond ((eql char #\\)
                      (setf escaped t))
                     ((member char delimiter)
                      (return-from nil)))
               (setf escaped nil))))))))

(defun naive-read-gen-list (stream end-marker)
  (append
   (list (list (read-char stream)))
   (iter
     (appending
      (slurp-whitespace stream))
     (until (eql (peek-char nil stream) end-marker))
     (appending (naive-read stream)))
   (list (list (read-char stream)))))

(defun naive-read-list (stream)
  (naive-read-gen-list stream #\)))

(defun dispatch-reader (stream)
  (list*
   (list (read-char stream))
   (slurp-number stream)
   (let ((char (read-char stream)))
     (case char
       ((#\+ #\-)
        (setf *form-found* t)
        (cons (list char)
              (append
               (naive-read stream)
               ;; The second invocation is to get the actual code rather than
               ;; the conditional
               (naive-read stream))))
       (#\(
        (setf *form-found* t)
        (cons (list char)
              (naive-read-list stream)))
       (#\\
        (setf *form-found* t)
        (cons (list char)
              (let ((peek (peek-char nil stream)))
                (cond ((member peek '(#\; #\, #\( #\) #\. #\' #\" #\` #\\ #\# #\|))
                       (list (list (read-char stream))))
                      (t (naive-read stream))))))
       (#\|
        (cons (list char)
              (read-sharp-bar-comment stream)))
       (#\# ;; do nothing
        (list (list #\#)))
       (otherwise
        (setf *form-found* t)
        (list* (list char)
               ;; Usually just a form.
               (naive-read stream)))))))

(defun slurp-number (stream)
  (when (member (peek-char nil stream) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (cons (read-char stream)
          (slurp-number stream))))

(defun read-sharp-bar-comment (stream)
  (list*
   (with-output-to-string (out)
     (block nil
       (iter (for char :in-stream-without-close stream :using #'read-char)
         (write-char char out)
         (when (and (eql char #\|) (eql (peek-char nil stream) #\#))
           (write-char (read-char stream) out)
           (return-from nil)))))
   ;; We need to read the next form because this wasn't a form, it was a comment.
   ;; We need to ignore any errors because there might not be any more forms to
   ;; read.
   (ignore-extra-closing-paren (naive-read stream))))

;; Special handling of cl-syntactic-sugar, cl-annot, and perhaps other packages.
;; It would be even better if I could provide a way to make this extensible.
;; These should only be turned on if that package is detected.

