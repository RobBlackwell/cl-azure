;;;; request.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; See Windows Azure Storage Services REST API Reference
;;; http://msdn.microsoft.com/en-us/library/dd179355.aspx

(defconstant +linefeed+ (string #\Linefeed))

(defun utf8 (string)
  "Converts a string to UTF8."
  (babel:string-to-octets string :encoding :utf-8))

(defun hmac-string (key string)
  "Computes a Hash-based Message Authentication Code (HMAC) from string using the SHA256 algorithm."
  (let ((hmac (ironclad:make-hmac 
	       (cl-base64:base64-string-to-usb8-array key) :sha256)))
    (ironclad:update-hmac hmac (utf8 string))
    (cl-base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun md5 (string)
  "Computes an MD5 hash of the given string."
  (cl-base64:usb8-array-to-base64-string
   (ironclad:digest-sequence :md5 (utf8 string))))

(defun authorization-header (account-name signature)
  "Returns an HTTP authorization header for Windows Azure Shared Key Lite format authorisation."
  (cons "Authorization" 
	(format nil "SharedKeyLite ~a:~a" account-name signature)))

(defun shared-key-authorization-header (account-name signature)
  "Returns an HTTP authorization header for Windows Azure Shared Key format authorisation."
  (cons "Authorization" 
	(format nil "SharedKey ~a:~a" account-name signature)))

(defun canonicalized-headers (request)
  "See http://msdn.microsoft.com/en-us/library/dd179428.aspx, Constructing the Canonicalized Headers String."
  (let ((headers (sort (copy-list (remove-if-not #'(lambda(x) (string= (subseq (string-downcase x) 0 5) "x-ms-")) 
						 (request-headers request) :key #'first))
		       #'string< :key #'first)))
    (format nil  "~{~a~}" (mapcar #'(lambda(x) (format nil "~a:~a~%" (first x) (rest x))) headers))))

;; Borrowed from Drakma
(defun dissect-query (query-string)
  "Accepts a query string as in PURI:URI-QUERY and returns a
corresponding alist of name/value pairs."
  (when query-string
    (loop for parameter-pair in (cl-ppcre:split "&" query-string)
          for (name value) = (cl-ppcre:split "=" parameter-pair :limit 2)
          collect (cons name value))))

(defun canonicalized-resource-1 (request)
    "See http://msdn.microsoft.com/en-us/library/dd179428.aspx, 2009-09-19 Shared Key Format."
    (let* ((url (puri:parse-uri (request-uri request)))
	   (options (dissect-query (puri:uri-query url))))
    (concatenate 'string 
		 "/" 
		 (account-name (request-account request))
		 (or (puri:uri-path url) "/")
		 (format nil  "~{~a~}" (mapcar #'(lambda(x) (format nil "~a~a:~a" 
								    +linefeed+ 
								    (string-downcase (first x)) 
								    (rest x))) 
					       (sort (copy-list options) #'string< :key #'first))))))

(defun canonicalized-resource-2 (request)
    "See http://msdn.microsoft.com/en-us/library/dd179428.aspx, 2009-09-19 Shared Key Lite and Table Service Format."
    (let ((uri (puri:parse-uri (request-uri request))))
    (concatenate 'string  
		 "/"
		 (account-name (request-account request))
		 (puri:uri-path uri)
		 (when (search "comp=list" (puri:uri-query uri))
		   "?comp=list")
		 (when (search "comp=metadata" (puri:uri-query uri))
		   "?comp=metadata")
		 )))

(defun string-to-sign-1  (request)
  "See http://msdn.microsoft.com/en-us/library/dd179428.aspx, Blob and Queue Services (Shared Key Authentication)."
  (let ((headers (request-headers request)))
    (concatenate 'string
		 (symbol-name (request-method request)) +linefeed+
		 (get-header headers "Content-Encoding") +linefeed+   
		 (get-header headers "Content-Language") +linefeed+   
		 (get-header headers "Content-Length") +linefeed+ 
		 (get-header headers "Content-MD5") +linefeed+
		 (get-header headers "Content-Type") +linefeed+    
		 (get-header headers "Date") +linefeed+
		 (get-header headers "If-Modified-Since") +linefeed+
		 (get-header headers "If-Match") +linefeed+
		 (get-header headers "If-None-Match") +linefeed+
		 (get-header headers "If-Unmodified-Since") +linefeed+
		 (get-header headers "range") +linefeed+
		 (canonicalized-headers request)         
		 (canonicalized-resource-1 request))))

(defun string-to-sign-2  (request)
  "See http://msdn.microsoft.com/en-us/library/dd179428.aspx,Table Service (Shared Key Authentication)."
  (let ((headers (request-headers request)))
    (concatenate 'string
		 (symbol-name (request-method request)) +linefeed+
		 (get-header headers "Content-MD5") +linefeed+
		 (get-header headers "Content-Type") +linefeed+    
		 (or (get-header headers "Date") (get-header headers "x-ms-date")) +linefeed+
		 (canonicalized-resource-2 request))))

(defun string-to-sign-3  (request)
  "See http://msdn.microsoft.com/en-us/library/dd179428.aspx, Blob and Queue Service (Shared Key Lite Authentication)."
  (let ((headers (request-headers request)))
    (concatenate 'string
		 (symbol-name (request-method request)) +linefeed+
		 (get-header headers "Content-MD5") +linefeed+
		 (get-header headers "Content-Type") +linefeed+    
		 (or (get-header headers "Date") (get-header headers "x-ms-date")) +linefeed+
		 (canonicalized-headers request)
		 (canonicalized-resource-2 request))))

(defun string-to-sign-4  (request)
  "See http://msdn.microsoft.com/en-us/library/dd179428.aspx,Table Service (Shared Key Lite Authentication)."
  (let ((headers (request-headers request)))
    (concatenate 'string
		 (or (get-header headers "Date") (get-header headers "x-ms-date")) +linefeed+
		 (canonicalized-resource-2 request))))

;; Broowed from ZS3
(defun url-decode (string)
  (with-output-to-string (out)
    (let ((code 0))
      (labels ((in-string (char)
                 (case char
                   (#\%
                    #'h1)
                   (t
                    (write-char char out)
                    #'in-string)))
               (weight (char)
                 (let ((weight (digit-char-p char 16)))
                   (unless weight
                     (error "~S is not a hex digit" char))
                   weight))
               (h1 (char)
                 (setf code (ash (weight char) 4))
                 #'h2)
               (h2 (char)
                 (incf code (weight char))
                 (write-char (code-char code) out)
                 #'in-string))
        (let ((state #'in-string))
          (loop for char across string
                do (setf state (funcall state char))))))))

