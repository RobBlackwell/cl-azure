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

(defun queue-storage-request (method resource &key (account *storage-account*) (content nil))
  "Makes an HTTP request to the Queue storage API"
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
		   (if (eq method :put)(cons "content-length" "0")) ;; PUT needs this to be set explicitly for some reason?
		   ))))

    (drakma:http-request (concatenate 'string (queue-storage-url account) resource)
			 :method method
			 :content content
			 :content-type nil
			 :additional-headers headers)))

(defun list-queues-raw (&key (account *storage-account*))
  "Makes a list-queues REST call to Azure Queue Storage"
  (queue-storage-request :get "/?comp=list" :account account))

(defun list-queues (&key (account *storage-account*))
  "Enumerates the queues in a storage account"
  (extract-named-elements (list-queues-raw :account account) "Name"))

(defun create-queue-raw (queue-name &key (account *storage-account*))
  "Creates a new queue under the given account"
  (queue-storage-request :put (format nil "/~a" queue-name) :account account))

(defun create-queue (queue-name &key (account *storage-account*))
  "Creates a new queue with the given name"
  (multiple-value-bind (body status)
      (create-queue-raw queue-name :account account)
    (eql status +http-created+)))

(defun delete-queue-raw (queue-name &key (account *storage-account*))
  "Deletes a queue"
  (queue-storage-request :delete (format nil "/~a" queue-name) :account account))

(defun delete-queue (queue-name &key (account *storage-account*))
  "Deletes a queue"
  (multiple-value-bind (body status)
      (delete-queue-raw queue-name :account account)
    (eql status +http-no-content+)))

;; get-queue-metadata - UNTESTED

;(defun get-queue-metadata-raw (queue-name &key (account *storage-account*))
;  "The Get Queue Metadata operation retrieves user-defined metadata and queue properties on the specified queue. Metadata is associated with the queue as name-values pairs."
;  (queue-storage-request :get (format nil "/~a?comp=metadata" queue-name) :account account))

;; set-queue-metadata


(defparameter *put-message-template*
"<QueueMessage>
    <MessageText>~a</MessageText>
</QueueMessage>")

(defun put-message-content (message)
  (format nil *put-message-template* message))

(defun put-message-raw (queue-name message &key (account *storage-account*))
  ""
  (queue-storage-request :post (format nil "/~a/messages" queue-name) :account account :content (put-message-content message)))

(defun put-message (queue-name message &key (account *storage-account*))
  ""
  (multiple-value-bind (body status)
      (put-message-raw queue-name message :account account)
    (eql status +http-created+)))

(defun get-message-raw (queue-name &key (account *storage-account*))
  ""
  (queue-storage-request :get (format nil "/~a/messages" queue-name) :account account))

(defun get-message (queue-name &key (account *storage-account*))
  ""
  (first (extract-rows (get-message-raw queue-name :account account) "QueueMessage")))


;; peek-message

;; delete-message


(defun clear-messages-raw (queue-name &key (account *storage-account*))
  "The Clear Messages operation deletes all messages from the specified queue."
  (queue-storage-request :delete (format nil "/~a/messages" queue-name) :account account))

(defun clear-messages (queue-name &key (account *storage-account*))
  "Clear all messages from the specified queue"
  (multiple-value-bind (body status)
      (clear-messages-raw queue-name :account account)
    (eql status +http-no-content+)))
