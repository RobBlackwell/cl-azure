;;;; cl-azure.asd
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(asdf:defsystem #:cl-azure
  :version "0.2.0"
  :author "Rob Blackwell"
  :description "Windows Azure support for Common Lisp."
  :serial t
  :depends-on (#:drakma
               #:ironclad
               #:cxml
               #:cl-base64
	       #:babel
	       #:cl-ppcre
	       #:puri
	       #:rt)
  :components ((:file "package")
	       (:file "util")
	       (:file "http")
	       (:file "handlers")
	       (:file "xml")
	       (:file "datetime")
	       (:file "edm")
               (:file "account")
	       (:file "request")
	       (:file "tables")
	       (:file "blobs")
	       (:file "queues")
	       (:file "manage")
	       (:file "acs")
	       (:file "tests")))

