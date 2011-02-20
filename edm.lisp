;;;; edm.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;;; Conceptual schema definition language (CSDL) supports a set of
;;; abstract primitive data types, called EDMSimpleTypes, that define
;;; properties in a conceptual model.

;;; http://msdn.microsoft.com/en-us/library/bb399548.aspx

(defconstant +edm-binary+ "Edm.Binary" "An array of bytes up to 64 KB in size")
(defconstant +edm-boolean+ "Edm.Boolean" "A Boolean value")
(defconstant +edm-datetime+ "Edm.DateTime" "A 64 bit value in UTC")
(defconstant +edm-double+ "Edm.Double" "A 64 bit floating point value")
(defconstant +edm-guid+ "Edm.Guid" "A 128-bit globally unique identifier")
(defconstant +edm-int32+ "Edm.Int32" "A 32-bit integer")
(defconstant +edm-int64+ "Edm.Int64" "A 64-bit integer")
(defconstant +edm-string+ "Edm.String" "A UTF-16-encoded of upto 64 KB in size")

