;;;; queues.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun queue-service-string-to-sign (method canonicalised-headers canonicalised-resource)
  "Returns the string to sign for the Queue service (Shared Key Lite Authentication)"
  (concatenate 'string (symbol-name method) 
	       +linefeed+ 
	       +linefeed+
	       +linefeed+ 
	       +linefeed+
	       canonicalised-headers
	       canonicalised-resource))

(defun queue-storage-request (method resource &optional (account *storage-account*))
  "Makes an HTTP request to the Queue storage API"
  (let* ((date (rfc1123-date-time-string))
	 (headers (list 
		   (authorization-header (account-name account)
					 (hmac-string (account-key account) 
						      (queue-service-string-to-sign 
						       method 
						       (canonicalised-headers date)
						       (canonicalise-resource resource account))))
		   (cons "x-ms-date" date)
		   (cons "x-ms-version" "2009-09-19")
		   (cons "DataServiceVersion" "1.0;NetFx") ; Needed for 2009-09-19
		   (cons "MaxDataServiceVersion" "2.0;NetFx") ; Needed for 2009-09-19
		   )))

    (drakma:http-request (concatenate 'string (queue-storage-url account) resource)
			 :method method
			 :additional-headers headers)))

(defun list-queues-raw (&key (account *storage-account*))
  "Makes a list-queues REST call to Azure Queue Storage"
  (queue-storage-request :get "/?comp=list" account))

(defun list-queues (&key (account *storage-account*))
  "Enumerates the queues in a storage account"
  (extract-named-elements (list-queues-raw :account account) "Name"))