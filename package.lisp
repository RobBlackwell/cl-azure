;;;; package.lisp

(defpackage #:cl-azure
  (:use #:cl)
  (:shadow "DEFCONSTANT")
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
   #:list-blobs)
  ;; Queues
  (:export
   #:list-queues))

