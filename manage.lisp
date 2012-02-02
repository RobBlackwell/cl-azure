;;;; manage.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;; Experimental
;; The Windows Azure Service Management API is a REST API for managing your services and deployments.
;; See http://msdn.microsoft.com/en-us/library/windowsazure/ee460799.aspx

(defconstant +service-management-version+ "2011-02-25")

(defparameter *subscription-id* "YOUR_SUBSCRIPTION_ID")

(defparameter *management-certificate* (list
				:certificate "~/YOUR_CERTIFICATE.pfx.pem"
				:key "~/YOUR_CERTIFICATE.pfx.pem"
				:pass-phrase "YOUR_PASSWORD"))

(defun management-request (method resource &key (content nil) 
			   (management-certificate *management-certificate*)
			   (handler #'identity))
  "Makes an HTTP request to the Windows Azure Service Management API"
  (let ((headers (list (cons "x-ms-version" +service-management-version+))))
    (funcall handler
    (web-request (list 
		  :certificate management-certificate
		  :method method
		  :uri resource
		  :content content
		  :content-type "application/xml"
		  :headers headers)))))

(defun list-service-name-elements-handler (response)
  "Returns a list of the elements named ServiceName if the HTTP status is ok, otherwise raises an error."
  (if (eq (response-status response) +HTTP-OK+)
    (extract-named-elements (response-body response) "ServiceName")
    (windows-azure-error response)))

(defun list-storage-accounts (&key (subscription-id *subscription-id*)
			      (management-certificate *management-certificate*)
			      (handler #'list-service-name-elements-handler))
  "Lists the storage accounts available for a subscription."
  (management-request :get (format nil "https://management.core.windows.net/~a/services/storageservices" subscription-id) 
		      :management-certificate management-certificate
		      :handler handler))

(defun list-hosted-services ( &key (subscription-id *subscription-id*) 
			     (management-certificate *management-certificate*)
			     (handler #'list-service-name-elements-handler))
  "Lists the hosted services available for a subscription."
  (management-request :get (format nil "https://management.core.windows.net/~a/services/hostedservices" subscription-id)
		      :management-certificate management-certificate
		      :handler handler))

(defun list-thumbprint-elements-handler (response)
  "Returns a list of the elements named ServiceName if the HTTP status is ok, otherwise raises an error."
  (if (eq (response-status response) +HTTP-OK+)
    (extract-named-elements (response-body response) "Thumbprint")
    (windows-azure-error response)))

(defun list-certificates (service-name &key (subscription-id *subscription-id*) 
			  (management-certificate *management-certificate*)
			  (handler #'list-thumbprint-elements-handler))
  "Lists all certificates associated with the specified hosted service."
  (management-request :get (format nil "https://management.core.windows.net/~a/services/hostedservices/~a/certificates" 
				   subscription-id service-name)
		       :management-certificate management-certificate
		       :handler handler))

