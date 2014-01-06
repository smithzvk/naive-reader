
;; (defpackage :iterate-extensions
;;     (:use :cl :iterate)
;;   (:export #:in-stream-without-close))

;; (in-package :iterate-extensions)

(defpackage :naive-reader
  (:use :cl :iterate)
  (:export
   #:read-form-string))

(in-package :naive-reader)

;; This is basically a quick hack from the Iterate code

(iter::defclause-driver (for var in-stream-without-close stream
                    &optional using (reader '#'read))
  "Forms in a stream (which will be left open at the end)"
  (iter::top-level-check)
  (return-stream-driver-code var stream reader :stream generate))

(defun return-stream-driver-code (var thing reader stream-or-file generate)
  (let* ((evar (iter::extract-var var))
	 (type (or (iter::var-type evar) t))
	 (stream-var (iter::make-var-and-binding 'stream nil))
	 (set-var (if (and (iter::var-spec? var)
			   (subtypep 'symbol type))
		      ;; We can use the given variable directly if no
		      ;; destructuring is required, and if the type of the
		      ;; variable can hold a symbol (since we use a gensym for
		      ;; the eof-marker).
		      evar
		      (iter::genvar 'element)))
	 (setq (cond ((eq set-var evar)
		      (iter::make-default-binding var) ())
		     (t (iter::make-default-binding set-var)
			(list (iter::do-dsetq var set-var)))))
	 (eof (gensym "EOF")))
    (setq iter::*loop-end-used?* t)
    (iter::return-driver-code
     :initial (if (eq stream-or-file :file)
		  `((setq ,stream-var (open ,thing :direction :input)))
		  `((setq ,stream-var ,thing)))
     :next `((if (eq (setq ,set-var ,(iter::make-funcall
				      reader stream-var nil `',eof))
		     ',eof) (go ,iter::*loop-end*))
	     .,setq)
     :final-protected `()
     :variable var)))
