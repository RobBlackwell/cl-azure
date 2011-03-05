;;;; queues.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

;; Define a protocol for queue messages

(defgeneric message-id (message))
(defgeneric pop-receipt (message))

;; Default implementation for queue messages is a plist

(defmethod message-id ((message cons))
  (getf message :|MessageId|))

(defmethod pop-receipt ((message cons))
  (getf message :|PopReceipt|))

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
			       (when (eq method :put) 
				 (cons "content-length" "0")) ;; PUT needs this to be set explicitly for some reason?
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

(defun get-queue-metadata-raw (queue-name &key (account *storage-account*))
  "The Get Queue Metadata operation retrieves user-defined metadata and queue properties on the specified queue. 
Metadata is associated with the queue as name-values pairs."
  (queue-storage-request :get (format nil "/~a?comp=metadata" queue-name) :account account))

(defun get-queue-metadata (queue-name &key (account *storage-account*))
  ""
  (multiple-value-bind (body status headers)
      (get-queue-metadata-raw queue-name :account account)
    headers))

(defun approximate-messages-count (queue-name &key (account *storage-account*))
  "Returns the approximate number of messages on the given queue"
  (nth-value 0 (parse-integer 
   (cdr (assoc :X-MS-APPROXIMATE-MESSAGES-COUNT (get-queue-metadata queue-name :account account))))))

;; TODO set-queue-metadata

(defparameter *put-message-template*
"<QueueMessage>
    <MessageText>~a</MessageText>
</QueueMessage>")

(defun put-message-content (message)
  (format nil *put-message-template* message))

(defun put-message-raw (queue-name message &key (account *storage-account*))
  "The Put Message operation adds a new message to the back of the message queue"
  (queue-storage-request :post (format nil "/~a/messages" queue-name) :account account :content (put-message-content message)))

(defun put-message (queue-name message &key (account *storage-account*))
  "The Put Message operation adds a new message to the back of the message queue."
  (multiple-value-bind (body status)
      (put-message-raw queue-name message :account account)
    (eql status +http-created+)))

(defun get-messages-raw (queue-name &key (account *storage-account*))
  "The Get Messages operation retrieves one or more messages from the front of the queue"
  (queue-storage-request :get (format nil "/~a/messages" queue-name) :account account))

(defun get-messages (queue-name &key (account *storage-account*))
  "The Get Messages operation retrieves one or more messages from the front of the queue"
  (extract-rows (get-messages-raw queue-name :account account) "QueueMessage"))

(defun peek-messages-raw (queue-name &key (account *storage-account*))
  "This operation retrieves one or more messages from the front of the queue, 
but does not alter the visibility of the message."
  (queue-storage-request :get (format nil "/~a/messages?peekonly=true" queue-name) :account account))

(defun peek-messages (queue-name &key (account *storage-account*))
  "This operation retrieves one or more messages from the front of the queue, 
but does not alter the visibility of the message."
  (extract-rows (peek-messages-raw queue-name :account account) "QueueMessage"))

(defun delete-message-raw (queue-name message-id pop-receipt &key (account *storage-account*))
  "The Delete Message operation deletes the specified message"
  (queue-storage-request :delete 
			 (format nil "/~a/messages/~a?popreceipt=~a" queue-name  message-id pop-receipt) 
			 :account account))

(defun delete-message (queue-name message &key (account *storage-account*))
  "Deletes a message from a queue"
  (multiple-value-bind (body status)
      (delete-message-raw queue-name (message-id message) (pop-receipt message) :account account)
    (eql status +http-no-content+)))

(defun clear-messages-raw (queue-name &key (account *storage-account*))
  "The Clear Messages operation deletes all messages from the specified queue."
  (queue-storage-request :delete (format nil "/~a/messages" queue-name) :account account))

(defun clear-messages (queue-name &key (account *storage-account*))
  "Clear all messages from the specified queue"
  (multiple-value-bind (body status)
      (clear-messages-raw queue-name :account account)
    (eql status +http-no-content+)))
