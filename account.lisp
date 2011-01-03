;;;; account.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; Define a protocol for dealing with Azure storage account
;;; information

(defgeneric account-name (account))
(defgeneric account-key (account))
(defgeneric table-storage-url (account))
(defgeneric blob-storage-url (account))
(defgeneric queue-storage-url (account))

;;; and a default implementation

(defparameter *storage-account* (list :account-name "myaccountname"
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
