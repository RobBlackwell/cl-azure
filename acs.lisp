;;;; acs.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(in-package :cl-azure)

;; Experimental support for the Access Control Service

;; http://msdn.microsoft.com/en-us/library/windowsazure/ee706734.aspx

(defparameter *plaintext-token-request-body-template*
"
wrap_scope=~a&
wrap_name=~a&
wrap_password=~a
")

;; Not yet working?

(defun plaintext-token-request (wrap-scope wrap-name wrap-password &key (handler #'identity))
  ""
  (funcall handler
	   (web-request (list
			 :method :post 
			 :uri (format nil "https://robblackwell.accesscontrol.windows.net/WRAPv0.9/")  
			 :body (format nil *plaintext-token-request-body-template*
				       wrap-scope wrap-name (base64:string-to-base64-string wrap-password))
			 :headers (acons "Content-type" "application/x-www-form-urlencoded" nil)))))



