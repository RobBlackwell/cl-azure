;;;; tests.lisp
;;;; Copyright (c) 2012, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

;; Unit tests

(rt:deftest test-hmac-string 
    (hmac-string "GuGbCQ41a9G1vtS1/dairlSMbXhHVzoA8+VPrbWxtj94o0aoAQdsgaaoYQASWqG9mj8xDvP1hSkvSVcLC34CfA==" "Hello World")
  "+UTfogPQ1ELBA4l+A7LwT1lbZVbP34F/CQzXaXqwfWA=")

(rt:deftest test-canonicalized-headers
    (string=
     (canonicalized-headers (list :headers (list (cons "x-ms-version" "2009-09-19") (cons "x-ms-date" "Sun, 12 Jun 2011 10:00:45 GMT"))))
     (concatenate 'string "x-ms-date:Sun, 12 Jun 2011 10:00:45 GMT"
		  +linefeed+
		  "x-ms-version:2009-09-19"
		  +linefeed+))
  t)

(rt:deftest test-canonicalized-resource-1
    (string=
     (canonicalized-resource-1 (list :account (list :account-name "myaccount") 
				     :uri "https://myaccount.blob.core.windows.net/mycontainer?restype=container&comp=metadata"))
     (concatenate 'string "/myaccount/mycontainer"
		  +linefeed+ "comp:metadata"
		  +linefeed+ "restype:container"))
  t)

(rt:deftest test2-canonicalized-resource-1
    (string=
     (canonicalized-resource-1 (list :account (list :account-name "myaccount") :uri "https://myaccount.blob.core.windows.net/?comp=list"))
     (concatenate 'string "/myaccount/"
		  +linefeed+ "comp:list"))
  t)

(rt:deftest test-query-tables
    (> (length (query-tables)) 0)
  t)

(rt:deftest test-list-containers
    (> (length (list-containers)) 0)
  t)

(rt:deftest test-list-queues
    (> (length (list-queues)) 0)
  t)

(rt:deftest test-list-hosted-services
    (> (length (list-hosted-services)) 0)
  t)

