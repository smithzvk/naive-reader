(use-package :stefil)

(deftest read-char-test ()
  (is (equal "#\\A" (read-form-string (make-string-input-stream "#\\A"))))
  (is (equal "#\\a" (read-form-string (make-string-input-stream "#\\a"))))
  (is (equal "#\\Space" (read-form-string (make-string-input-stream "#\\Space")))))

(deftest read-comment-test ()
  (is (equal '(";; hello" nil) (multiple-value-list (read-form-string ";; hello"))))
  (is (equal '(";; hello
A" t) (multiple-value-list (read-form-string ";; hello
A"))))
  (is (equal '("#|hello|#" nil) (multiple-value-list (read-form-string "#|hello|#")))))

(deftest reader-conditional-test ()
  (is (equal '("#+sbcl blah" t)
             (multiple-value-list (read-form-string "#+sbcl blah"))))
  (is (equal '("#+(or sbcl) blah" t)
             (multiple-value-list (read-form-string "#+(or sbcl) blah")))))

(deftest read-list-test ()
  (is (equal '("(sbcl ecl cmucl)" t)
             (multiple-value-list (read-form-string "(sbcl ecl cmucl)")))))

(deftest read-quote-test ()
  (is (equal '("|(sbcl ecl cmucl)|" t)
             (multiple-value-list (read-form-string "|(sbcl ecl cmucl)|"))))
  (is (equal '("\"sbcl ecl cmucl\"" t)
             (multiple-value-list (read-form-string "\"sbcl ecl cmucl\"")))))

;; (deftest reader-dispatch-test ()
;;   (read-char-test)
;;   (reader-conditional-test))
