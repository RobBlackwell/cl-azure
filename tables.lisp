;;;; tables.lisp
;;;; Copyright (c) 2011 - 2013, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun sign-table-storage-request (request)
  "Signs a table storage request by computing a shared key signature and adding a corresponding Authorization header."
  (let* ((str (string-to-sign-2 request))
	 (account (request-account request))
	 (key (account-key account)))
    (add-header request
		(shared-key-authorization-header (account-name account)
						 (hmac-string key str)))))

(defun table-storage-request (method resource &key (account *storage-account*) 
			      (content nil) 
			      (headers nil)
			      (handler #'identity))
  "Makes an HTTP request to the Table storage API."
  (funcall handler
	   (web-request 
	    (sign-table-storage-request 
		   (list :method method 
			 :uri (format nil "~a~a" (table-storage-url account) resource)
			 :headers (acons "x-ms-date" (rfc1123-date-time-string)
					 (acons "x-ms-version" "2011-08-18"
						(acons "DataServiceVersion" "1.0;NetFx" ; Needed for 2009-09-19
						       (acons "MaxDataServiceVersion" "2.0;NetFx" ; Needed for 2009-09-19
							      headers))))
			 :body content
			 :account account)))))

(defun list-tablename-elements-handler (response)
  "Extracts a list of tables from an ADO.NET entity set Atom feed response body."
  (if (eq (response-status response) +HTTP-OK+)
    (extract-named-elements (response-body response) "TableName")
    (windows-azure-error response)))

(defun query-tables (&key (account *storage-account*) (handler #'list-tablename-elements-handler))
  "Enumerates the tables in a storage account."
  (table-storage-request :get "/Tables()" 
			 :account account 
			 :handler handler))

(defun rows-handler (response)
  "Extracts a list of table rows from an HTTP Response."
  (if (eq (response-status response) +HTTP-OK+)
    (extract-rows (response-body response) "properties")
    (windows-azure-error response)))

(defun query-entities (table-name &key (account *storage-account*)
		       (partition-key nil) 
		       (row-key nil) 
		       (filter nil)
		       (select nil)
		       (handler #'rows-handler))
  "Queries data in a table."
  (let ((partition-key-expression (if partition-key (format nil "PartitionKey='~a'" partition-key) ""))
	(row-key-expression (if row-key (format nil ",RowKey='~a'" row-key) ""))
	(filter-expression (if filter (format nil "?$filter=~a" filter) ""))
	(select-expression (if select (format nil "&?$select=~a" select) "")))
    (table-storage-request :get (concatenate 'string "/" table-name "(" partition-key-expression row-key-expression ")"
					     filter-expression select-expression) 
			   :account account
			   :handler handler)))

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

(defun create-table (table-name &key (account *storage-account*) (handler #'created-handler))
  "Creates a table."
  (let ((content (format nil *create-table-template* table-name)))
    (table-storage-request :post "/Tables" 
			   :account account 
			   :headers (list (cons "Content-Type" "application/atom+xml"))
			   :content content
			   :handler handler)))

(defun ensure-table (table-name &key (account *storage-account*) (handler #'ensure-created-handler))
  "Ensures that a table exists, if necessary creates it."
  (create-table table-name :account account :handler handler))

(defun delete-table (table-name &key (account *storage-account*) (handler #'no-content-handler))
  "Deletes the specified table and any data it contains."
  (table-storage-request :delete (format nil "/Tables('~a')" table-name) 
			 :account account
			 :handler handler))

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
  ""
  (with-output-to-string (stream)
    (loop for x on entity by #'cddr do
      (format stream (property-xml (first x) (to-edm-string (second x)) (edm-type-description (second x)))))))

(defun entity-content (entity)
  ""
  (format nil *insert-entity-template* (iso8601-date-time-string) (properties-xml entity)))
  
(defun insert-entity (table-name entity &key (account *storage-account*) (handler #'created-handler))
  "Inserts a new entity into a table."
  (table-storage-request :post (format nil "/~a" table-name) 
			 :account account 
			 :headers (acons "Content-Type" "application/atom+xml" nil)
			 :content (entity-content entity)
			 :handler handler))

;; E.g. (insert-entity "People" '(|PartitionKey| 55 |RowKey| 4 "Name" "Robert" "Age" 21 ))


;; update entity

;; merge entity

;; delete entity
