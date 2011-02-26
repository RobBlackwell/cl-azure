;;;; xml.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; Some utilities for working with cxml and klacks

(defun find-next-child (source &optional (flag nil))
  "Finds the next child in the klacks source, or returns nil if no more children"
  (let ((element (klacks:peek-next source)))
    (cond 
      ((eq element :start-element) t)
      ((eq element :end-element)
       (if flag nil
	   (find-next-child source t)))
      (t (find-next-child source flag)))))

(defun extract-named-elements (doc name)
  "Extracts a list of all values from elements named name within an XML document doc"
  (let ((source (cxml:make-source doc)))
    (loop 
       while (klacks:find-element source name)
       do  (klacks:peek-next source)
       collecting (klacks:current-characters source))))

(defun print-xml (doc &optional (stream t))
  "Prints an XML document where doc might be a string, octets or anything acceptable to cxml:make-source"
  (let ((source (cxml:make-source doc))
	(handler (cxml:make-character-stream-sink stream :indentation 1)))
    (klacks:serialize-source source handler)))

(defun extract-rows (doc rowname)
  "Returns a list of plists being rows identified by rowname"
  (klacks:with-open-source (source (cxml:make-source doc))
    (loop 
       while (klacks:find-element source rowname)
       collect 
       (loop
	  while (find-next-child source)
	  append (list (intern (klacks:current-lname source) "KEYWORD")
			   (progn (klacks:peek-next source)
				  (klacks:current-characters source)))))))