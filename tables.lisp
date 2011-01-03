;;;; tables.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun table-service-string-to-sign (date-time canonicalized-resource)
  "Returns the string to sign for the Table service (Shared Key Lite Authentication)"
  (concatenate 'string date-time 
	       (string #\Linefeed) 
	       canonicalized-resource))

(defun table-storage-request (method resource &optional (account *storage-account*))
  "Makes an HTTP request to the Table storage API"
  (let* ((date (rfc1123-date-time-string))
	 (headers (list 
		   (authorization-header (account-name account)
					 (hmac-string (account-key account) 
						      (table-service-string-to-sign date 
										    (canonicalise-resource resource account))))
		   (cons "x-ms-date" date)
		   (cons "x-ms-version" "2009-09-19")
		   (cons "DataServiceVersion" "1.0;NetFx") ; Needed for 2009-09-19
		   (cons "MaxDataServiceVersion" "2.0;NetFx") ; Needed for 2009-09-19
		   )))

    (drakma:http-request (concatenate 'string (table-storage-url account) resource)
			 :method method
			 :additional-headers headers)))

(defun extract-tables (response)
  "Extracts a list of tables from an ADO.NET entity set Atom feed"
  (extract-named-elements response "TableName"))

(defun query-tables-raw (&key (account *storage-account*))
  "Makes a Query Tables REST call to Azure Table Storage"
  (table-storage-request :get "/Tables()" account))

(defun query-tables (&key (account *storage-account*))
  "Enumerates the tables in a storage account"
  (extract-tables (query-tables-raw :account account)))

(defun extract-rows (response)
  "Extracts a list of rows from an ADO.NET entity set Atom feed"
  (klacks:with-open-source (source (cxml:make-source response))
    (loop 
       while (klacks:find-element source "properties")
       collect 
       (loop
	  while (find-next-child source)
	  collecting (cons (intern (klacks:current-lname source))
			   (progn (klacks:peek-next source)
				  (klacks:current-characters source)))))))

(defun query-entities-raw (table-name &key (account *storage-account*)
			   (partition-key nil) 
			   (row-key nil) 
			   (filter nil))
  "Makes a Query Entities REST call to Azure Table Storage"
  (let ((filter-expression (if filter (format nil "?$filter=~a" filter) "")))
    (table-storage-request :get (concatenate 'string "/" table-name "()" filter-expression) account)))

(defun query-entities (table-name &key (account *storage-account*)
		       (partition-key nil) 
		       (row-key nil) 
		       (filter nil))
  "Queries data in a table returning rows as a list of alists"
  (extract-rows (query-entities-raw table-name 
				  :account account 
				  :partition-key partition-key 
				  :row-key row-key 
				  :filter filter)))