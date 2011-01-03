;;;; package.lisp

(defpackage #:cl-azure
  (:use #:cl)
  (:export
   #:*storage-account*)
  ;; Tables
  (:export
   #:query-tables
   #:query-entities)
  ;; Blobs
  (:export
   #:list-containers
   #:list-blobs))

