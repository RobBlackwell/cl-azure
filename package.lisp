;;;; package.lisp

(defpackage #:cl-azure
  (:use #:cl)
  (:export
   #:*storage-account*)
  ;; Tables
  (:export
   #:query-tables
   #:create-table
   #:ensure-table
   #:delete-table
   #:query-entities)
  ;; Blobs
  (:export
   #:list-containers
   #:list-blobs))

