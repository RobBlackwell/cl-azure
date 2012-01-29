;;;; blobs.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defun sign-blob-storage-request (request)
  "Signs a blob storage request by computing a shared key signature and adding a corresponding Authorization header."
  (let* ((str (string-to-sign-1 request))
	 (account (request-account request))
	 (key (account-key account)))
    (add-header request
		(shared-key-authorization-header (account-name account)
						 (hmac-string key str)))))

(defun blob-storage-request (method resource &key (account *storage-account*) 
			     (content nil) 
			     (headers nil) 
			     (handler #'identity))
  "Makes an HTTP request to the Blob storage API."
  (funcall handler
	   (web-request 
	    (sign-blob-storage-request 
			 (list :method method 
			       :uri (format nil "~a~a" (blob-storage-url account) resource)
			       :headers (acons "x-ms-date" (rfc1123-date-time-string) 
					 (acons "x-ms-version" "2009-09-19" headers))
			       :body content
			       :account account)))))

(defun list-containers (&key (account *storage-account*) (handler #'list-name-elements-handler))
  "Enumerates the containers in a storage account."
  (blob-storage-request :get "/?comp=list" 
			:account account 
			:handler handler))

(defun list-blobs (container &key (account *storage-account*) (handler #'list-name-elements-handler))
  "Enumerates the blobs in a storage container."
  (blob-storage-request :get (concatenate 'string "/" container "?restype=container&comp=list") 
			:account account 
			:handler handler))

(defun delete-blob (container-name blob-name &key (account *storage-account*) (handler #'accepted-handler))
  "Deletes a blob."
  (blob-storage-request :delete (format nil "/~a/~a" container-name blob-name) 
			:account account
			:handler handler))

(defun create-container (container-name &key (account *storage-account*) (handler #'created-handler))
  "Creates a new container."
  (blob-storage-request :put (format nil "/~a?restype=container" container-name) 
			:account account
			:headers (acons "Content-Length" "0" nil)
			:handler handler))

(defun delete-container (container-name &key (account *storage-account*) (handler #'accepted-handler))
  "Deletes a container."
  (blob-storage-request :delete (format nil "/~a?restype=container" container-name) 
			:account account
			:handler handler))

(defun get-blob (container-name blob-name &key (account *storage-account*) (handler #'get-body-handler))
  "Gets a Blob from the specificed container."
  (blob-storage-request :get (format nil "/~a/~a" container-name blob-name) 
			:account account
			:handler handler))

(defun get-blob-string (container-name blob-name &key (account *storage-account*))
  "Gets a Blob and returns it as a UTF8 string."
  (my-utf8-bytes-to-string (get-blob container-name blob-name 
				     :account account 
				     :handler #'get-body-handler )))

(defun get-blob-file (container-name blob-name file-name &key (account *storage-account*) (if-exists :error))
  "Gets a Blob and saves it to a file."
  (with-open-file (stream file-name :direction :output :if-exists if-exists :element-type '(unsigned-byte 8))
    (write-sequence (get-blob container-name blob-name :account account :handler #'get-body-handler) stream)))


;; get-container-properties

;; get-container-metadata

;; set-container-metadata

;; get-container-acl

;; set-container-acl

(defun put-blob (container-name blob-name content &key (account *storage-account*) 
		 (blob-type "BlockBlob")
		 (handler #'created-handler))
  "Creates a new block blob or page blob."
  (blob-storage-request :put (format nil "/~a/~a" container-name blob-name)
			:account account
			:content content
			:headers (acons "x-ms-blob-type" blob-type 
					(acons "Content-Length" (format nil "~a" (length content)) nil))
			:handler handler))

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








