;;;; util.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; Courtesy Edi Weitz
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; Courtesy Rainer Joswig
(defun merge-plist (p1 p2)
  "Merges two property lists"
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
        do (progn
             (push value p2)
             (push indicator p2)))
  p2)

(defconstant +utf8-bom+ (vector #xEF #xBB #xBF) "Byte Order Mark for UTF-8")

(defun my-utf8-bytes-to-string (bytes)
  "Convert a byte array to a UTF-8 string, skipping the byte order mark if necessary"
  (if (equalp (subseq bytes 0 3) +utf8-bom+)
      (babel:octets-to-string bytes :start 3 :encoding :utf-8)
      (babel:octets-to-string bytes :encoding :utf-8)))