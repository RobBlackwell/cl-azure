;;;; http.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;; Model the HTTP Protocol as specified in RFC2616

;; Request

(defgeneric request-method (request) (:documentation "Returns the request method from an HTTP request."))
(defgeneric request-uri (request) (:documentation "Returns the uri from an HTTP request."))
(defgeneric request-headers (request) (:documentation "Returns the headers from an HTTP request."))
(defgeneric request-body (request) (:documentation "Returns the body from an HTTP request."))

;; Response

(defgeneric response-status (response) (:documentation "Returns the status code from an HTTP response."))
(defgeneric response-body (response) (:documentation "Returns the body from an HTTP response."))
(defgeneric response-headers (response) (:documentation "Returns the headers from an HTTP response."))

;; Provide a default implementation of the model based on a plist

;; Request

(defmethod request-method ((request cons))
  (getf request :method))

(defmethod request-uri ((request cons))
  (getf request :uri))

(defmethod request-headers ((request cons))
  (getf request :headers))

(defmethod request-body ((request cons))
  (getf request :body))

;; Response

(defmethod response-status ((response cons))
  (getf response :status))

(defmethod response-headers ((response cons))
  (getf response :headers))

(defmethod response-body ((response cons))
  (getf response :body))

;; headers modelled as alists

(defun get-header (headers name)
  ""
  (rest (assoc name headers :test #'string=)))

(defun add-header (request header)
  ""
  (merge-plist request (list :headers (cons header (request-headers request)))))

;; Add the notion of a client certificate to http-requests
(defgeneric request-client-certificate (request))

(defmethod request-client-certificate ((request cons))
  (getf request :certificate))

;; Define a protocol for interacting with X509 client certificates

(defgeneric client-certificate-certificate (client-certificate))
(defgeneric client-certificate-key (client-certificate))
(defgeneric client-certificate-pass-phrase (client-certificate))

;; And a simple default implementation

(defmethod client-certificate-certificate ((client-certificate cons))
  (getf client-certificate :certificate))

(defmethod client-certificate-key ((client-certificate cons))
  (getf client-certificate :key))

(defmethod client-certificate-pass-phrase ((client-certificate cons))
  (getf client-certificate :pass-phrase))

;; Drakma Implementation

(defun web-request (request)
  "Uses Drakma to make the specificed http request."
  (setf *foo* request)
  (if (request-client-certificate request)
      (multiple-value-bind  (body status headers)
	  (drakma:http-request (request-uri request) 
			       :method (request-method request) 
			       :additional-headers (request-headers request) 
			       :content (request-body request)
			       :certificate (client-certificate-certificate (request-client-certificate request))
			       :key (client-certificate-key (request-client-certificate request))
			       :certificate-password (client-certificate-pass-phrase (request-client-certificate request)))
	(list :body body :status status :headers headers))
      (multiple-value-bind  (body status headers)
	  (drakma:http-request (request-uri request) 
			       :method (request-method request) 
			       :additional-headers (request-headers request) 
			       :content (request-body request))
	(list :body body :status status :headers headers))))

;; HTTP Status codes

(defconstant +http-continue+ 100 "Continue")
(defconstant +http-switching-protocols+ 101 "Switching Protocols")
(defconstant +http-ok+ 200 "OK")
(defconstant +http-created+ 201 "Created")
(defconstant +http-accepted+ 202 "Accepted")
(defconstant +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(defconstant +http-no-content+ 204 "No Content")
(defconstant +http-reset-content+ 205 "Reset Content")
(defconstant +http-partial-content+ 206 "Partial Content")
(defconstant +http-multi-status+ 207 "Multi-Status")
(defconstant +http-multiple-choices+ 300 "Multiple Choices")
(defconstant +http-moved-permanently+ 301 "Moved Permanently")
(defconstant +http-moved-temporarily+ 302 "Found")
(defconstant +http-see-other+ 303 "See Other")
(defconstant +http-not-modified+ 304 "Not Modified")
(defconstant +http-use-proxy+ 305 "Use Proxy")
(defconstant +http-temporary-redirect+ 307 "Temporary Redirect")
(defconstant +http-bad-request+ 400 "Bad Request")
(defconstant +http-authorization-required+ 401 "Unauthorized")
(defconstant +http-payment-required+ 402  "Payment Required")
(defconstant +http-forbidden+ 403 "Forbidden")
(defconstant +http-not-found+ 404 "Not Found")
(defconstant +http-method-not-allowed+ 405 "Method Not Allowed")
(defconstant +http-not-acceptable+ 406 "Not Acceptable")
(defconstant +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(defconstant +http-request-time-out+ 408 "Request Timeout")
(defconstant +http-conflict+ 409 "Conflict")
(defconstant +http-gone+ 410 "Gone")
(defconstant +http-length-required+ 411 "Length Required")
(defconstant +http-precondition-failed+ 412 "Precondition Failed")
(defconstant +http-request-entity-too-large+ 413 "Request Entity Too Large")
(defconstant +http-request-uri-too-large+ 414 "Request-URI Too Large")
(defconstant +http-unsupported-media-type+ 415 "Unsupported Media Type")
(defconstant +http-requested-range-not-satisfiable+ 416 "Requested Range Not Satisfiable")
(defconstant +http-expectation-failed+ 417 "Expectation Failed")
(defconstant +http-failed-dependency+ 424 "Failed Dependency")
(defconstant +http-internal-server-error+ 500 "Internal Server Error")
(defconstant +http-not-implemented+ 501 "Not Implemented")
(defconstant +http-bad-gateway+ 502 "Bad Gateway")
(defconstant +http-service-unavailable+ 503 "Service Unavailable")
(defconstant +http-gateway-time-out+ 504 "Gateway Timeout")
(defconstant +http-version-not-supported+ 505 "HTTP Version Not Supported")