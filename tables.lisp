;;;; tables.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun table-service-string-to-sign (date-time canonicalized-resource)
  "Returns the string to sign for the Table service (Shared Key Lite Authentication)"
  (concatenate 'string date-time 
	       +linefeed+ 
	       canonicalized-resource))

(defun table-storage-request (method resource &key (account *storage-account*) (content nil))
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
			 :content content
			 :content-type "application/atom+xml"
			 :additional-headers headers)))

(defun extract-tables (response)
  "Extracts a list of tables from an ADO.NET entity set Atom feed"
  (extract-named-elements response "TableName"))

(defun query-tables-raw (&key (account *storage-account*))
  "Makes a Query Tables REST call to Azure Table Storage"
  (table-storage-request :get "/Tables()" :account account))

(defun query-tables (&key (account *storage-account*))
  "Enumerates the tables in a storage account"
  (extract-tables (query-tables-raw :account account)))

(defun query-entities-raw (table-name &key (account *storage-account*)
			   (partition-key nil) 
			   (row-key nil) 
			   (filter nil))
  "Makes a Query Entities REST call to Azure Table Storage"
  (let ((filter-expression (if filter (format nil "?$filter=~a" filter) "")))
    (table-storage-request :get (concatenate 'string "/" table-name "()" filter-expression) :account account)))

(defun query-entities (table-name &key (account *storage-account*)
		       (partition-key nil) 
		       (row-key nil) 
		       (filter nil))
  "Queries data in a table returning rows as a list of alists"
  (extract-rows (query-entities-raw table-name 
				  :account account 
				  :partition-key partition-key 
				  :row-key row-key 
				  :filter filter) "properties"))

(defparameter *create-table-template*
  "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>   
  <entry xmlns:d=\"http://schemas.microsoft.com/ado/2007/08/dataservices\" 
    xmlns:m=\"http://schemas.microsoft.com/ado/2007/08/dataservices/metadata\"
    xmlns=\"http://www.w3.org/2005/Atom\"> 
    <title /> 
    <updated>2009-03-18T11:48:34.9840639-07:00</updated> 
    <author>
      <name/> 
    </author> 
      <id/> 
      <content type=\"application/xml\">
        <m:properties>
          <d:TableName>~a</d:TableName>
        </m:properties>
      </content> 
    </entry>")

(defun create-table-raw (table-name &key (account *storage-account*))
  "The Create Table operation creates a new table in the storage account."
  (table-storage-request :post "/Tables" :account account :content (format nil *create-table-template* table-name)))

(defun create-table (table-name &key (account *storage-account*))
  "Creates a table withte gie name, returns T on success, nil otherwise"
(multiple-value-bind (body status)
      (create-table-raw table-name :account account)
    (eql status +http-created+)))

(defun ensure-table (table-name &key (account *storage-account*))
  "Ensures the table exists, if necessary creating it"
  (multiple-value-bind (body status)
      (create-table-raw table-name :account account)
    (when (member status (list +http-created+ +http-conflict+)) status)))

(defun delete-table-raw (table-name &key (account *storage-account*))
  "The Delete Table operation deletes the specified table and any data it contains."
  (table-storage-request :delete (format nil "/Tables('~a')" table-name) :account account ))

(defun delete-table (table-name &key (account *storage-account*))
  "Deletes the specified table and any data it contains, return T on success, NIL otherwise"
  (multiple-value-bind (body status)
      (delete-table-raw table-name :account account)
    (eql status +http-no-content+)))

(defparameter *insert-entity-template*
  "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>
<entry xmlns:d=\"http://schemas.microsoft.com/ado/2007/08/dataservices\" xmlns:m=\"http://schemas.microsoft.com/ado/2007/08/dataservices/metadata\" xmlns=\"http://www.w3.org/2005/Atom\">
  <title />
  <updated>~a</updated>
  <author>
    <name />
  </author>
  <id />
  <content type=\"application/xml\">
    <m:properties>
~a
    </m:properties>
  </content>
</entry>")

(defun stringify (x)
  (if (symbolp x)
      (symbol-name x)
      x))

(defun type-xml (type)
  (if type
      (format nil " m:type=\"~a\"" (stringify type))
      ""))

(defun property-xml (name value &optional (type nil))
    (let ((tag-name (stringify name)))
      (format nil "<d:~a~a>~a</d:~a>" tag-name (type-xml type) value tag-name)))

(defun properties-xml (entity)
  (with-output-to-string (stream)
    (dolist (property entity)
      (format stream (property-xml (first property) (second property) (third property))))))

(defun entity-content (entity)
  (format nil *insert-entity-template* (iso8601-date-time-string) (properties-xml entity)))
  
(defun insert-entity-raw (table-name entity &key (account *storage-account*))
  "The Insert Entity operation inserts a new entity into a table"
  (table-storage-request :post (format nil "/~a" table-name) :account account :content (entity-content entity)))

;; (insert-entity-raw "People" '((|PartitionKey| 3)(|RowKey| 4)("Name" "Robert") ("Age" 21 |Edm.Int32|)))