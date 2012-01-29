;;;; queues.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

;; Define a protocol for queue messages

(defgeneric message-id (message))
(defgeneric pop-receipt (message))

;; Default implementation for queue messages based on a plist

(defmethod message-id ((message cons))
  (getf message :|MessageId|))

(defmethod pop-receipt ((message cons))
  (getf message :|PopReceipt|))

;;

(defun sign-queue-storage-request (request)
  "Signs a queue storage request by computing a shared key signature and adding a corresponding Authorization header."
  (let* ((str (string-to-sign-1 request))
	 (account (request-account request))
	 (key (account-key account)))
    (add-header request
		(shared-key-authorization-header (account-name account)
						 (hmac-string key str)))))

(defun queue-storage-request (method resource &key (account *storage-account*) 
			      (content nil) 
			      (headers nil)
			      (handler #'identity))
  "Makes an HTTP request to the Queue storage API."
  (funcall handler
	   (web-request
		  (sign-queue-storage-request 
		   (list :method method 
			 :uri (format nil "~a~a" (queue-storage-url account) resource)
			 :headers (acons "x-ms-date" (rfc1123-date-time-string) 
					 (acons "x-ms-version" "2011-08-18"
						headers))
			 :body content
			 :account account)))))

(defun list-queues (&key (account *storage-account*) (handler #'list-name-elements-handler))
  "Enumerates the queues in a storage account."
  (queue-storage-request :get "/?comp=list" :account account :handler handler))

(defun create-queue (queue-name &key (account *storage-account*) (handler #'created-handler))
  "Creates a new queue with the given name."
  (queue-storage-request :put (format nil "/~a" queue-name) :account account :handler handler))

(defun delete-queue (queue-name &key (account *storage-account*) (handler #'no-content-handler))
  "Deletes a queue"
  (queue-storage-request :delete (format nil "/~a" queue-name) :account account :handler handler))

(defun get-queue-metadata (queue-name &key (account *storage-account*) (handler #'get-headers-handler))
  "Retrieves user-defined metadata and queue properties on the specified queue."
  (queue-storage-request :get (format nil "/~a?comp=metadata" queue-name) :account account :handler handler))

(defun approximate-messages-count (queue-name &key (account *storage-account*))
   "Returns the approximate number of messages on the given queue."
   (nth-value 0 (parse-integer 
    (cdr (assoc :X-MS-APPROXIMATE-MESSAGES-COUNT (get-queue-metadata queue-name :account account
								     :handler #'get-headers-handler))))))

;; TODO set-queue-metadata

(defparameter *put-message-template*
"<QueueMessage>
    <MessageText>~a</MessageText>
</QueueMessage>")

(defun put-message-content (message)
  (format nil *put-message-template* message))

(defun put-message (queue-name message &key (account *storage-account*) (handler #'created-handler))
  "Adds a new message to the back of the message queue."
  (let ((content (put-message-content message)))
    (queue-storage-request :post (format nil "/~a/messages" queue-name) 
			   :account account 
			   :content content
			   :headers (acons "Content-Type" "application/atom+xml" nil)
			   :handler handler)))

(defun messages-handler (response)
  ""
  (if (eq (response-status response) +HTTP-OK+)
    (extract-rows (response-body response) "QueueMessage")
    (windows-azure-error response)))

(defun get-messages (queue-name &key (account *storage-account*) (handler #'messages-handler))
  "Retrieves one or more messages from the front of the queue."
  (queue-storage-request :get (format nil "/~a/messages" queue-name) 
			 :account account
			 :handler handler))

(defun peek-messages (queue-name &key (account *storage-account*) (handler #'messages-handler))
  "Retrieves one or more messages from the front of the queue, but does not alter the visibility of the message."
  (queue-storage-request :get (format nil "/~a/messages?peekonly=true" queue-name) 
			 :account account
			 :handler handler))

(defun delete-message (queue-name message-id pop-receipt &key (account *storage-account*) (handler #'no-content-handler))
  "Deletes the specified message."
  (queue-storage-request :delete 
			 (format nil "/~a/messages/~a?popreceipt=~a" queue-name message-id 
				 pop-receipt
				 ;;(drakma::url-encode pop-receipt :utf8)
				 ) 
			 :account account
			 :handler handler))

(defun clear-messages (queue-name &key (account *storage-account*) (handler #'no-content-handler))
  "Deletes all messages from the specified queue."
  (queue-storage-request :delete (format nil "/~a/messages" queue-name) 
			 :account account 
			 :handler handler))

