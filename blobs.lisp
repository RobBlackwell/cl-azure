;;;; blobs.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun blob-service-string-to-sign (method canonicalised-headers canonicalised-resource)
  "Returns the string to sign for the Blob service (Shared Key Lite Authentication)"
  (concatenate 'string (symbol-name method) 
	       +linefeed+ 
	       +linefeed+
	       +linefeed+ 
	       +linefeed+
	       canonicalised-headers
	       canonicalised-resource))

(defun blob-storage-request (method resource &key (account *storage-account*) (content nil))
  "Makes an HTTP request to the Blob storage API"
  (let* ((date (rfc1123-date-time-string))
	 (headers (remove nil (list 
			       (authorization-header (account-name account)
						     (hmac-string (account-key account) 
								  (queue-service-string-to-sign 
								   method 
								   (canonicalised-headers date)
								   (canonicalise-resource resource account))))
			       (cons "x-ms-date" date)
			       (cons "x-ms-version" "2009-09-19")
			       (when (eq method :put) 
				 (cons "content-length" "0")) ;; PUT needs this to be set explicitly for some reason?
			       ))))

    (drakma:http-request (concatenate 'string (blob-storage-url account) resource)
			 :method method
			 :additional-headers headers)))

(defun list-containers-raw (&key (account *storage-account*))
  "Makes a list-containers REST call to Azure Blob Storage"
  (blob-storage-request :get "/?comp=list" :account account))

(defun list-containers (&key (account *storage-account*))
  "Enumerates the containers in a storage account"
  (extract-named-elements (list-containers-raw :account account) "Name"))

(defun list-blobs-raw (container &key (account *storage-account*))
  "Makes a list-blobs REST call to Azure Blob Storage"
  (blob-storage-request :get (concatenate 'string "/" container "?restype=container&comp=list") :account account))

(defun list-blobs (container &key (account *storage-account*))
  "Enumerates the blobs in a storage account"
  (extract-named-elements (list-blobs-raw container :account account) "Name"))

(defun delete-blob-raw (container-name blob-name &key (account *storage-account*))
  "Deletes a blob"
  (blob-storage-request :delete (format nil "/~a/~a" container-name blob-name) :account account))

(defun delete-blob (container-name blob-name &key (account *storage-account*))
  "Deletes a blob"
  (multiple-value-bind (body status)
      (delete-blob-raw container-name blob-name :account account)
    (eql status +http-accepted+)))

(defun create-container-raw (container-name &key (account *storage-account*))
  "Creates a new container"
  (blob-storage-request :put (format nil "/~a?restype=container" container-name) :account account))

(defun create-container (container-name &key (account *storage-account*))
  "Creates a new container"
  (multiple-value-bind (body status)
      (create-container-raw container-name :account account)
    (eql status +http-created+)))

(defun delete-container-raw (container-name &key (account *storage-account*))
  "Deletes a container"
  (blob-storage-request :delete (format nil "/~a?restype=container" container-name) :account account))

(defun delete-container (container-name &key (account *storage-account*))
  "Deletes a container"
  (multiple-value-bind (body status)
      (delete-container-raw container-name :account account)
    (eql status +http-accepted+)))

(defun get-blob-raw (container-name blob-name &key (account *storage-account*))
  "The Get Blob operation reads or downloads a blob from the system, including its metadata and properties. You can also call Get Blob to read a snapshot."
  (blob-storage-request :get (format nil "/~a/~a" container-name blob-name) :account account))

(defun get-blob (container-name blob-name &key (account *storage-account*))
  "Returns a byte array representing the contents of the specified blob"
  (multiple-value-bind (body status)
      (get-blob-raw container-name blob-name :account account)
    (when (eql status +http-ok+)
      body)))

(defconstant +utf8-bom+ (vector #xEF #xBB #xBF) "Byte Order Mark for UTF-8")

(defun my-utf8-bytes-to-string (bytes)
  "Convert a byte array to a UTF-8 string, skipping the byte order mark if necessary"
  (if (equalp (subseq bytes 0 3) +utf8-bom+)
      (babel:octets-to-string bytes :start 3 :encoding :utf-8)
      (babel:octets-to-string bytes :encoding :utf-8)))

(defun get-blob-text (container-name blob-name &key (account *storage-account*))
  ""
  (my-utf8-bytes-to-string (get-blob container-name blob-name :account account)))

;(defun get-blob-file (container-name blob-name file-name &key (account *storage-account*))
;  ""
;  (with-open-file (stream file-name :direction :output :if-exists :supersede)
;    (write-sequence (get-blob container-name blob-name :account account) stream)))


;; get-container-properties

;; get-container-metadata

;; set-container-metadata

;; get-container-acl

;; set-container-acl

;; put-blob

;; get-blob-properties

;; set-blob-properties

;; get-blob-metadata

;; set-blob-metatdata

;; lease-blob

;; snapshot-blob

;; copy-blob

;; put-block

;; put-block-list

;; get-block-list

;; put-page

;; get-page-regions