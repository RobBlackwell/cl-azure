;;;; package.lisp

(defpackage #:cl-azure
  (:use #:cl)
  (:shadow "DEFCONSTANT")
  (:export
   #:*storage-account*
   #:account-from-environment)
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
   #:list-blobs
   #:delete-blob
   #:create-container
   #:delete-container
   #:get-blob)
  ;; Queues
  (:export
   #:list-queues
   #:create-queue
   #:delete-queue
   #:get-queue-metadata
   #:approximate-messages-count
   #:put-message
   #:get-messages
   #:peek-messages
   #:delete-message
   #:clear-messages))

