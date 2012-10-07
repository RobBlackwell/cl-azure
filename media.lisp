;;;; media.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Media Services REST API Reference
;;; See http://msdn.microsoft.com/en-us/library/windowsazure/hh973618.aspx

(defconstant +media-oauth-url+ "https://wamsprodglobal001acs.accesscontrol.windows.net/v2/OAuth2-13")
(defconstant +media-api-url+ "https://wamsdubclus001rest-hs.cloudapp.net/api/")

;;; Define a protocol for dealing with Windows Azure Media Services credentials

(defgeneric media-account-name (account))
(defgeneric media-account-key (account))
(defgeneric media-oauth-url (account))
(defgeneric media-api-url (account))
(defgeneric media-access-token (account))

;;; and a default implementation

(defparameter *media-account* (list :media-account-name "YOUR_ACCOUNT"
				    :media-account-key "YOUR_KEY"
				    :media-oauth-url +media-oauth-url+
				    :media-api-url +media-api-url+))

(defun make-media-account (name key)
  ""
  (list :media-account-name name
	:media-account-key key
	:media-oauth-url +media-oauth-url+
	:media-api-url +media-api-url+))

(defmethod media-account-name ((media-account cons))
  (getf media-account :media-account-name))

(defmethod media-account-key ((media-account cons))
  (getf media-account :media-account-key))

(defmethod media-oauth-url ((media-account cons))
  (getf media-account :media-oauth-url))

(defmethod media-api-url ((media-account cons))
  (getf media-account :media-api-url))

(defun json-handler (response)
  "Handles and decodes an HTTP response in JSON format."
  (if (eq (response-status response) +HTTP-OK+)
      (json:decode-json-from-string (my-utf8-bytes-to-string (response-body response)))
      response))

;;; The REST API

(defparameter *get-media-token-template*
  "grant_type=client_credentials&client_id=~a&client_secret=~a&scope=urn%3aWindowsAzureMediaServices")

(defun get-media-token (&key (account *media-account*)(handler #'json-handler))
  "Requests a token from the OAuth v2 endpoint of ACS."
  (let ((content (format nil *get-media-token-template* (media-account-name account) (drakma::url-encode (media-account-key account) :utf-8))))
    (funcall handler 
	     (web-request
	      (list :method :post
		    :uri (media-oauth-url account)
		    :headers (list (cons "Content-Type" "application/x-www-form-urlencoded")
				   (cons "Expect" "100-continue")
				   (cons "Connection" "Keep-Alive"))
		    :body content)))))

(defmethod media-access-token ((media-account cons))
  (let ((token (getf media-account :media-token)))
    (if token
	(assoc :access--token  token)
	(rest (assoc :access--token (get-media-token :account media-account))))))

(defun media-request (method resource &key (media-account *media-account*) 
		      (content nil) 
		      (handler #'json-handler))
  "Makes an HTTP request to the Media Services REST API."
  (let ((token (media-access-token media-account)))
    (funcall handler
	     (web-request 
	      (list :method method 
		    :uri (format nil "~a~a" (media-api-url media-account) resource)
		    :headers (list (cons "Content-Type" "application/json;odata=verbose")
				   (cons "Accept" "application/json;odata=verbose")
				   (cons "DataServiceVersion" "3.0")
				   (cons "MaxDataServiceVersion" "3.0")
				   (cons "x-ms-version" "1.0")
				   (cons "Authorization" (format nil "Bearer ~a" token)))
		    :body content)))))

(defun get-media-processors(&key (media-account *media-account*) (handler #'json-handler))
  (media-request :get "MediaProcessors" :media-account media-account :handler handler))

(defun get-media-assets (&key (media-account *media-account*) (handler #'json-handler))
  (media-request :get "Assets" :media-account media-account :handler handler))

(defun get-media-jobs(&key (media-account *media-account*) (handler #'json-handler))
  (media-request :get "Jobs" :media-account media-account :handler handler))


