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
	       #:babel
	       #:puri)
  :components ((:file "package")
	       (:file "util")
	       (:file "http")
	       (:file "xml")
	       (:file "datetime")
	       (:file "edm")
               (:file "account")
	       (:file "request")
	       (:file "tables")
	       (:file "blobs")
	       (:file "queues")))

