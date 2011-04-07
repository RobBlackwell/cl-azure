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

(defgeneric edm-type-description (object))
(defgeneric to-edm-string (object))

(defmethod edm-type-description ((self t))
  nil)

(defmethod to-edm-string ((self t))
  (format nil "~a" self))

(defclass edm-object ()
  ((value :reader value :initarg :value)
   (edm-type-description :reader edm-type-description :initform nil))
  (:documentation "An Entity Data Model type"))

(defmethod print-object ((self edm-object) stream)
  (print-unreadable-object (self stream :type t)
    (princ (value self) stream)))

(defmethod to-edm-string ((self edm-object))
  (format nil "~a" (value self)))

(defclass edm-int64 (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-int64+))
  (:documentation "A 64-bit integer"))

(defun make-edm-int64 (value)
  (if (stringp value)
      (make-instance 'edm-int64 :value (read-from-string value))
      (make-instance 'edm-int64 :value value)))

(defclass edm-int32 (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-int32+))
  (:documentation "A 32-bit integer"))

(defun make-edm-int32 (value)
  (if (stringp value)
      (make-instance 'edm-int32 :value (read-from-string value))
      (make-instance 'edm-int32 :value value)))

(defclass edm-double (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-double+))
  (:documentation ""))

(defun make-edm-double (value)
  (if (stringp value)
      (make-instance 'edm-double :value (read-from-string value))
      (make-instance 'edm-double :value value)))

(defclass edm-string (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-string+))
  (:documentation ""))

(defmethod to-edm-string ((self edm-string))
  (value self))

(defun make-edm-string (value)
  (if (stringp value)
      (make-instance 'edm-string :value value)
      (make-instance 'edm-double :value (format nil "~a" value))))

(defclass edm-boolean (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-boolean+))
  (:documentation ""))

(defmethod to-edm-string ((self edm-boolean))
  (if (value self) "true" "false"))

(defun make-edm-boolean (value)
   (if (stringp value)
       (make-instance 'edm-boolean :value (string= value "true"))
       (make-instance 'edm-boolean :value value)))

(defclass edm-guid (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-guid+))
  (:documentation ""))

(defmethod to-edm-string ((self edm-guid))
  (value self))

(defun make-edm-guid (value)
  (make-instance 'edm-guid :value value))

(defclass edm-datetime (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-datetime+))
  (:documentation ""))

(defmethod to-edm-string ((self edm-datetime))
  (value self))

(defun make-edm-datetime (value)
  (make-instance 'edm-datetime :value value))

(defclass edm-binary (edm-object)
  ((edm-type-description :reader edm-type-description :initform +edm-binary+))
  (:documentation ""))

(defmethod to-edm-string ((self edm-binary))
  ;;(trivial-utf-8:utf-8-bytes-to-string (value self))
  (babel:octets-to-string (value self) :encoding :utf-8))

(defun make-edm-binary (value)
  (if (stringp value)
      ;;(make-instance 'edm-binary :value (trivial-utf-8:string-to-utf-8-bytes value))
      (make-instance 'edm-binary :value (babel:string-to-octets value :encoding :utf-8))
      (make-instance 'edm-binary :value value)))

(defun make-edm-object (object &optional (edm-description nil))
  ""
  (if edm-description
      (cond
	((string= edm-description +edm-binary+) (make-edm-binary object))
	((string= edm-description +edm-boolean+) (make-edm-boolean object))
	((string= edm-description +edm-datetime+) (make-edm-datetime object))
	((string= edm-description +edm-double+) (make-edm-double object))
	((string= edm-description +edm-guid+) (make-edm-guid object))
	((string= edm-description +edm-int32+) (make-edm-int32 object))
	((string= edm-description +edm-int64+) (make-edm-int64 object))
	((string= edm-description +edm-string+) (make-edm-string object))
	(t object))
      (typecase object
	(float (make-edm-double object))
	(string (make-edm-string object))
	(boolean (make-edm-boolean object))
	(null (make-edm-boolean object))
	(bit (make-edm-int32 object))
	(integer (make-edm-int64 object))
	((SIMPLE-ARRAY (UNSIGNED-BYTE 8) *) (make-edm-binary object))
	(t (make-edm-string object)))))

