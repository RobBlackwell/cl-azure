;;;; servicebus.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;;; Experimental support for the Windows Azure Service Bus
;;;; http://msdn.microsoft.com/en-us/library/windowsazure/ee732537.aspx

;;; Define a protocol for dealing with Servicebus credentials

(defgeneric service-namespace (servicebus-credentials))
(defgeneric issuer-name (servicebus-credentials))
(defgeneric issuer-secret (servicebus-credentials))
(defgeneric servicebus-url (servicebus-credentials))
(defgeneric acs-url (servicebus-credentials))
(defgeneric realm (servicebus-credentials))
(defgeneric token (servicebus-credentials))

;; and a default implementation

(defun make-servicebus-credentials (service-namespace issuer-name issuer-secret)
  (list 
   :service-namespace service-namespace
   :issuer-name issuer-name
   :issuer-secret issuer-secret
   :servicebus-url (format nil "https://~a.servicebus.windows.net" service-namespace)
   :acs-url (format nil "https://~a-sb.accesscontrol.windows.net" service-namespace)
   ;; Note that the realm used when requesting a token uses the HTTP scheme, even though
   ;; calls to the service are always issued over HTTPS
   :realm (format nil "http://~a.servicebus.windows.net/" service-namespace)))

(defparameter *servicebus-credentials* (make-servicebus-credentials "YOURNAMESPACE" "owner" "YOURISSUERSECRET"))

(defmethod service-namespace ((servicebus-credentials cons))
  (getf servicebus-credentials :service-namespace))

(defmethod issuer-name ((servicebus-credentials cons))
  (getf servicebus-credentials :issuer-name))

(defmethod issuer-secret ((servicebus-credentials cons))
  (getf servicebus-credentials :issuer-secret))

(defmethod servicebus-url ((servicebus-credentials cons))
  (getf servicebus-credentials :servicebus-url))

(defmethod acs-url ((servicebus-credentials cons))
  (getf servicebus-credentials :acs-url))

(defmethod realm ((servicebus-credentials cons))
  (getf servicebus-credentials :realm))

(defmethod token ((servicebus-credentials cons))
  (getf servicebus-credentials :token))

;;

(defun extract-token (response)
  "Extracts an ACS supplied SimpleWebToken from an ACS response and
formats it as a WRAPv0.9.7.2 token for use with the Service Bus REST API."
  (format nil "WRAP access_token=\"~a\""
	  (url-decode
	   (second
	    (cl-ppcre:split "="
			    (first
			     (cl-ppcre:split "&"
					     (babel:octets-to-string
					      (response-body response)))))))))


(defun get-token (&key (servicebus-credentials *servicebus-credentials*) (handler #'extract-token))
  "Calls the Access Control Service to authenticate the given Service
Bus credentials."
  (funcall handler
	   (plaintext-token-request (acs-url servicebus-credentials)
				    (realm servicebus-credentials)
				    (issuer-name servicebus-credentials)
				    (issuer-secret servicebus-credentials))))

(defparameter *servicebus-send-message-body-template*
"<entry xmlns='http://www.w3.org/2005/Atom'>
  <content type='application/xml'>~a</content>
</entry>
")

(defun servicebus-send-message (topic-path message &key (servicebus-credentials *servicebus-credentials*) (handler #'created-handler))
  "Sends a new messages to a topic or queue."
  ;; http://msdn.microsoft.com/en-us/library/windowsazure/hh780718.aspx
  (funcall handler
	   (web-request (list
			 :method :post 
			 :uri (format nil "~a/~a/messages" (servicebus-url servicebus-credentials) topic-path)  
			 :body (format nil *servicebus-send-message-body-template* message)
			 :headers (acons "Authorization" (token servicebus-credentials) 
					 (acons "Content-Type" "application/atom+xml;type=entry;charset=utf-8" nil))))))

(defun servicebus-get-entity (topic-path &key (servicebus-credentials *servicebus-credentials*) (handler #'identity))
  ""
  ;; http://msdn.microsoft.com/en-us/library/windowsazure/hh780754.aspx
  (funcall handler
	   (web-request (print (list
			 :method :get 
			 :uri (format nil "~a/~a" (servicebus-url servicebus-credentials) topic-path)  
			 :headers (acons "Authorization" (token servicebus-credentials) nil))))))


(defun servicebus-list-queues ( &key (servicebus-credentials *servicebus-credentials*) (handler #'identity))
  "Enumerates the Service Bus queues."
  ;; http://msdn.microsoft.com/en-us/library/windowsazure/hh780759.aspx
  (funcall handler
	   (web-request (print (list
			 :method :get 
			 :uri (format nil "~a/$Resources/Queues" (servicebus-url servicebus-credentials))  
			 :headers (acons "Authorization" (token servicebus-credentials) nil))))))

(defun servicebus-list-topics ( &key (servicebus-credentials *servicebus-credentials*) (handler #'identity))
  "Enumerates the Service bus topics."
  (funcall handler
	   (web-request (print (list
			 :method :get 
			 :uri (format nil "~a/$Resources/Topics" (servicebus-url servicebus-credentials))  
			 :headers (acons "Authorization" (token
  servicebus-credentials) nil))))))

;; (setf (getf *servicebus-credentials* :token) (get-token))