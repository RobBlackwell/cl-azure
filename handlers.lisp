;;;; handlers.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;; Commonly used functions for processing HTTP responses.

(defun windows-azure-error (response)
  "Raises an error using the Message from the response body."
  (error (extract-named-elements (response-body response) "Message")))

(defun get-body-handler (response)
  "Returns the response body if the HHTP status is ok, otherwise raises an error."
  (if (eq (response-status response) +http-ok+)
      (response-body response)
      (windows-azure-error response)))

(defun get-headers-handler (response)
  "Returns the respone headers if the HTTP status is ok, otherwise raises an error."
  (if (eq (response-status response) +http-ok+)
      (response-headers response)
      (windows-azure-error response)))

(defun list-name-elements-handler (response)
  "Returns a list of the elements named Name if the HTTP status is ok, otherwise raises an error."
  (if (eq (response-status response) +HTTP-OK+)
    (extract-named-elements (response-body response) "Name")
    (windows-azure-error response)))

(defun created-handler (response)
  "Returns true if the HTTP response is ok, otherwise raises an error."
  (if (eq (response-status response) +http-created+)
      t
      (windows-azure-error response)))

(defun accepted-handler (response)
  "Returns true if the HTTP response is accepted, otherwise raises an error."
  (if (eq (response-status response) +http-accepted+)
      t
      (windows-azure-error response)))

(defun no-content-handler (response)
  "Returns true if the HTTP response is no-content, otherwise raises an error."
  (if (eq (response-status response) +http-no-content+)
      t
      (windows-azure-error response)))

(defun ensure-created-handler (response)
  "returns true if the HTTP response is created, otherwise raises an error."
  (if (member (response-status response) (list +http-created+ +http-conflict+))
      t
      (windows-azure-error response)))
