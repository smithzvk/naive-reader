
(asdf:defsystem #:naive-reader
  :author "Zach Kost-Smith"
  :license "LLGPL"
  :serial t
  :components ((:file "iterate-extensions")
               (:file "naive-reader"))
  :depends-on (:iterate :stefil))
