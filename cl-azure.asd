;;;; cl-azure.asd
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(asdf:defsystem #:cl-azure
  :version "0.2.1"
  :author "Rob Blackwell"
  :description "A Windows Azure library for Common Lisp."
  :serial t
  :depends-on (#:drakma
               #:ironclad
               #:cxml
               #:cl-base64
	       #:babel
	       #:cl-ppcre
	       #:puri
	       #:cl-json
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
	       (:file "servicebus")
	       (:file "media")
	       (:file "tests")))

