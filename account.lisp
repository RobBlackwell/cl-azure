;;;; account.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; Windows Azure Dev Storage default credentials
(defconstant +devstore-account+ "devstoreaccount1")
(defconstant +devstore-key+ "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==")
(defconstant +devstore-blob-url+ "http://127.0.0.1:10000/devstoreaccount1")
(defconstant +devstore-table-url+ "http://127.0.0.1:10002/devstoreaccount1")
(defconstant +devstore-queue-url+  "http://127.0.0.1:10001/devstoreaccount1")

;;; Define a protocol for dealing with Azure storage account
;;; information

(defgeneric account-name (account))
(defgeneric account-key (account))
(defgeneric table-storage-url (account))
(defgeneric blob-storage-url (account))
(defgeneric queue-storage-url (account))

;;; and a default implementation

(defparameter *storage-account* (list :account-name +devstore-account+
				:account-key +devstore-key+
				:table-storage-url 
				+devstore-table-url+
				:blob-storage-url 
				+devstore-blob-url+
				:queue-storage-url 
				+devstore-queue-url+))

(defparameter *sample-account* (list :account-name "myaccountname"
				:account-key "myaccountkey"
				:table-storage-url 
				"http://myaccount.table.core.windows.net"
				:blob-storage-url 
				"http://myaccount.blob.core.windows.net"
				:queue-storage-url 
				"http://myaccount.queue.core.windows.net"))

(defmethod account-name ((account cons))
  (getf account :account-name))

(defmethod account-key ((account cons))
  (getf account :account-key))

(defmethod table-storage-url ((account cons))
  (getf account :table-storage-url))

(defmethod blob-storage-url ((account cons))
  (getf account :blob-storage-url))

(defmethod queue-storage-url ((account cons))
  (getf account :queue-storage-url))

;; Based on http://cl-cookbook.sourceforge.net/os.html
(defun portable-getenv (name &optional default)
  "Returns the named operating system environment variable"
  #+CMU
  (let ((x (assoc name ext:*environment-list*
		  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun account-from-environment()
  "Reads the Azure account details from the underlying OS variables AZURE_ACCOUNT_NAME and AZURE_ACCOUNT_KEY"
  (let ((account-name (portable-getenv "AZURE_ACCOUNT_NAME" +devstore-account+))
	(account-key (portable-getenv "AZURE_ACCOUNT_KEY" +devstore-key+)))
    (list :account-name account-name
	  :account-key account-key
	  :table-storage-url 
	  (format nil "http://~a.table.core.windows.net" account-name)
	  :blob-storage-url 
	  (format nil "http://~a.blob.core.windows.net" account-name)
	  :queue-storage-url 
	  (format nil "http://~a.queue.core.windows.net" account-name))))
  
  