;;;; blobs.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun blob-service-string-to-sign (method canonicalised-headers canonicalised-resource)
  "Returns the string to sign for the Blob service (Shared Key Lite Authentication)"
  (concatenate 'string method 
	       +linefeed+ 
	       +linefeed+
	       +linefeed+ 
	       +linefeed+
	       canonicalised-headers
	       canonicalised-resource))

(defun blob-storage-request (method resource &optional (account *storage-account*))
  "Makes an HTTP request to the Blob storage API"
  (let* ((date (rfc1123-date-time-string))
	 (headers (list 
		   (authorization-header (account-name account)
					 (hmac-string (account-key account) 
						      (blob-service-string-to-sign 
						       "GET" 
						       (canonicalised-headers date)
						       (canonicalise-resource resource account))))
		   (cons "x-ms-date" date)
		   (cons "x-ms-version" "2009-09-19")
		   (cons "DataServiceVersion" "1.0;NetFx") ; Needed for 2009-09-19
		   (cons "MaxDataServiceVersion" "2.0;NetFx") ; Needed for 2009-09-19
		   )))

    (drakma:http-request (concatenate 'string (blob-storage-url account) resource)
			 :method method
			 :additional-headers headers)))

(defun list-containers-raw (&key (account *storage-account*))
  "Makes a list-containers REST call to Azure Blob Storage"
  (blob-storage-request :get "/?comp=list" account))

(defun extract-containers (response)
  "Extracts a list of containers from an ADO.NET entity set Atom feed"
  (extract-named-elements response "Name"))

(defun list-containers (&key (account *storage-account*))
  "Enumerates the containers in a storage account"
  (extract-containers (list-containers-raw :account account)))

(defun list-blobs-raw (container &key (account *storage-account*))
  "Makes a list-blobs REST call to Azure Blob Storage"
  (blob-storage-request :get (concatenate 'string "/" container "?restype=container&comp=list") account))

(defun extract-blobs (response)
  "Extracts a list of blobs from an ADO.NET entity set Atom feed"
  (extract-named-elements response "Name"))

(defun list-blobs (blob &key (account *storage-account*))
  "Enumerates the blobs in a storage account"
  (extract-blobs (list-blobs-raw blob :account account)))