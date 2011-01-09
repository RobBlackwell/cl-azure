;;;; request.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defconstant +linefeed+ (string #\Linefeed))

(defun utf8 (string)
  "Converts a string to UTF8"
  (trivial-utf-8:string-to-utf-8-bytes string))

(defun hmac-string (key string)
  "Computes a Hash-based Message Authentication Code (HMAC) from string using the SHA256 algorithm"
  (let ((hmac (ironclad:make-hmac 
	       (cl-base64:base64-string-to-usb8-array key) :sha256)))
    (ironclad:update-hmac hmac (utf8 string))
    (cl-base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun authorization-header (account-name signature)
  "Returns an HTTP authorization header for SharedKeyLite"
  (cons "Authorization" 
	(format nil "SharedKeyLite ~a:~a" account-name signature)))

(defun canonicalise-resource (resource &optional (account *storage-account*))
  "Canonicalise the resource using 2009-09-19 Shared Key Lite and Table Service Format"
  (let ((url (puri:parse-uri (concatenate 'string "/" (account-name account) resource ))))
    (concatenate 'string  
		 (puri:uri-path url)
		 (when (search "comp" (puri:uri-query url))
		   "?comp=list"))))

(defun canonicalised-headers (date)
  "Return canonicalised headers using 2009-09-19 Shared Key Lite and Table Service Format"
  (concatenate 'string "x-ms-date:" date +linefeed+  "x-ms-version:"  "2009-09-19" +linefeed+))