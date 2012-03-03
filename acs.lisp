;;;; acs.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(in-package :cl-azure)

;; Experimental support for the Access Control Service

;; http://msdn.microsoft.com/en-us/library/windowsazure/ee706734.aspx

(defparameter *plaintext-token-request-body-template*
"wrap_scope=~a&wrap_name=~a&wrap_password=~a")

(defun plaintext-token-request (acs-url wrap-scope wrap-name wrap-password &key (handler #'identity))
  "Requests a token from the Access Control Service."
  (funcall handler
	   (web-request (print (list
				:method :post 
				:uri (format nil "~a/WRAPv0.9/" acs-url)  
				:body (format nil *plaintext-token-request-body-template*
					      (drakma::url-encode wrap-scope :utf-8)
					      (drakma::url-encode wrap-name :utf-8)
					      (drakma::url-encode wrap-password :utf-8))
				:headers (acons "Content-Type" "application/x-www-form-urlencoded" nil))))))



