;;;; package.lisp
;;;; Copyright (c) 2011 - 2012, Rob Blackwell.  All rights reserved.

(defpackage #:cl-azure
  (:nicknames "WA")
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
   #:query-entities
   #:insert-entity)
  ;; Blobs
  (:export
   #:list-containers
   #:list-blobs
   #:delete-blob
   #:create-container
   #:delete-container
   #:get-blob
   #:get-blob-string
   #:get-blob-file
   #:put-blob)
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
   #:clear-messages)
  ;; Management
  (:export
   #:list-storage-accounts
   #:list-hosted-services
   #:list-certificates)
  ;; Media
  (:export
   #:make-media-account
   #:get-media-token
   #:get-media-assets
   #:get-media-jobs
   #:get-media-processors))

