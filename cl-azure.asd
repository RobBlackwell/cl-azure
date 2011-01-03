;;;; cl-azure.asd

(asdf:defsystem #:cl-azure
  :version "0.1"
  :author "Rob Blackwell"
  :description "Windows Azure support for Common Lisp"
  :serial t
  :depends-on (#:drakma
               #:ironclad
               #:cxml
               #:cl-base64
               #:trivial-utf-8
	       #:puri)
  :components ((:file "package")
	       (:file "xml")
	       (:file "datetime")
               (:file "account")
	       (:file "request")
	       (:file "tables")
	       (:file "blobs")))

