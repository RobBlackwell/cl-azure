cl-azure
========

Access Microsoft Windows Azure cloud storage from Common Lisp.

Introduction
------------

Windows Azure is Microsoft's cloud hosting platform. This project is
an incomplete, proof-of-concept implementation of a Common Lisp
library for accessing the storage features, namely Blobs, Tables and
Queues.

Demo / Instructions
-------------------


     ; SLIME 2010-11-03
     CL-USER> (ql:quickload "cl-azure")
     ...
     ("cl-azure")
     CL-USER> (in-package #:cl-azure)
     #<PACKAGE "CL-AZURE">
     CL-AZURE> *storage-account*
     (:ACCOUNT-NAME "myaccountname" :ACCOUNT-KEY "myaccountkey" :TABLE-STORAGE-URL
      "http://myaccount.table.core.windows.net" :BLOB-STORAGE-URL
       "http://myaccount.blob.core.windows.net" :QUEUE-STORAGE-URL
       "http://myaccount.queue.core.windows.net")
     CL-AZURE> (load "config") ; to set *storage-account* to my own account details
     T
     CL-AZURE> (list-containers)
     ("cdn" "drives" "foo" "junk" "old" "packages" "phpapps" "printer" "public"
      "vsdeploy" "wad-control-container")
     CL-AZURE> (list-blobs "drives")
     ("RD00155D3A0380.vhd" "RD00155D3A06AA.vhd" "RD00155D3A0BDB.vhd"
      "RD00155D3A0EF0.vhd" "RD00155D3A120B.vhd" "RD00155D3A1693.vhd"
      "RD00155D3A39DA.vhd" "RD00155D3A3A3D.vhd")
     CL-AZURE> (query-tables)
     ("People" "WADLogsTable" "WADPerformanceCountersTable"
      "WADWindowsEventLogsTable")
     CL-AZURE> (query-entities "People")
     (((|PartitionKey| . "1") (|RowKey| . "1")
       (|Timestamp| . "2010-12-23T18:56:31.7762335Z") (|Name| . "Rob")
       (|Age| . "41"))
      ((|PartitionKey| . "2") (|RowKey| . "2")
       (|Timestamp| . "2010-12-23T18:56:31.2673916Z") (|Name| . "Ed") (|Age| . "5")))

     CL-AZURE> (setf drakma:*HEADER-STREAM* *standard-output*) ; show the HTTP conversation
     #<SWANK-BACKEND::SLIME-OUTPUT-STREAM {10037B0951}>
     CL-AZURE> (query-tables-raw) ; raw response
     GET /Tables() HTTP/1.1
     Host: robblackwell.table.core.windows.net
     User-Agent: Drakma/1.2.3 (SBCL 1.0.29; Darwin; 10.5.0; http://weitz.de/drakma/)
     Accept: */*
     Connection: close
     Authorization: SharedKeyLite robblackwell:3O71sJKwJ2btPHVvL87X/++g3UToXpsusUJpPk0hCug=
     x-ms-date: Mon, 03 Jan 2011 16:43:45 GMT
     x-ms-version: 2009-09-19
     DataServiceVersion: 1.0;NetFx
     MaxDataServiceVersion: 2.0;NetFx
     
     HTTP/1.1 200 OK
     Cache-Control: no-cache
     Transfer-Encoding: chunked
     Content-Type: application/atom+xml;charset=utf-8
     Server: Windows-Azure-Table/1.0 Microsoft-HTTPAPI/2.0
     x-ms-request-id: 828a7678-f1e1-4d77-aedd-adaacab4cf0f
     x-ms-version: 2009-09-19
     Date: Mon, 03 Jan 2011 16:43:46 GMT
     Connection: close
     
     #(60 63 120 109 108 32 118 101 114 115 105 111 110 61 34 49 46 48 34 32 101 110
     99 111 100 105 110 103 61 34 117 116 102 45 56 34 32 115 116 97 110 100 97
     ...
     115 62 13 10 32 32 32 32 60 47 99 111 110 116 101 110 116 62 13 10 32 32 60
     47 101 110 116 114 121 62 13 10 60 47 102 101 101 100 62)
     200
     ((:CACHE-CONTROL . "no-cache") (:TRANSFER-ENCODING . "chunked")
      (:CONTENT-TYPE . "application/atom+xml;charset=utf-8")
      (:SERVER . "Windows-Azure-Table/1.0 Microsoft-HTTPAPI/2.0")
      (:X-MS-REQUEST-ID . "828a7678-f1e1-4d77-aedd-adaacab4cf0f")
      (:X-MS-VERSION . "2009-09-19") (:DATE . "Mon, 03 Jan 2011 16:43:46 GMT")
      (:CONNECTION . "close"))
     #<PURI:URI https://robblackwell.table.core.windows.net/Tables()>
     #<FLEXI-STREAMS:FLEXI-IO-STREAM {1005CA2781}>
     T
     "OK"
     CL-AZURE> (print-xml *) ; xml response body
     <?xml version="1.0" encoding="UTF-8"?>
     <feed xmlns="http://www.w3.org/2005/Atom" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata" xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices" xml:base="https://robblackwell.table.core.windows.net/"> 
      <title type="text">
       Tables</title> 
      <id>
       https://robblackwell.table.core.windows.net/Tables</id> 
      <updated>
       2011-01-03T16:43:46Z</updated> 
      <link href="Tables" title="Tables" rel="self"/> 
      <entry> 
       <id>
	https://robblackwell.table.core.windows.net/Tables('People')</id> 
       <title type="text"/> 
       <updated>
	2011-01-03T16:43:46Z</updated> 
       <author> 
	<name/> 
       </author> 
       <link href="Tables('People')" title="Tables" rel="edit"/> 
       <category scheme="http://schemas.microsoft.com/ado/2007/08/dataservices/scheme" term="robblackwell.Tables"/> 
       <content type="application/xml"> 
	<m:properties> 
	 <d:TableName>
	  People</d:TableName> 
	</m:properties> 
       </content> 
      </entry> 
      <entry> 
       <id>
	https://robblackwell.table.core.windows.net/Tables('WADLogsTable')</id> 
       <title type="text"/> 
       <updated>
	2011-01-03T16:43:46Z</updated> 
       <author> 
	<name/> 
       </author> 
       <link href="Tables('WADLogsTable')" title="Tables" rel="edit"/> 
       <category scheme="http://schemas.microsoft.com/ado/2007/08/dataservices/scheme" term="robblackwell.Tables"/> 
       <content type="application/xml"> 
	<m:properties> 
	 <d:TableName>
	  WADLogsTable</d:TableName> 
	</m:properties> 
       </content> 
      </entry> 
      <entry> 
       <id>
	https://robblackwell.table.core.windows.net/Tables('WADPerformanceCountersTable')</id> 
       <title type="text"/> 
       <updated>
	2011-01-03T16:43:46Z</updated> 
       <author> 
	<name/> 
       </author> 
       <link href="Tables('WADPerformanceCountersTable')" title="Tables" rel="edit"/> 
       <category scheme="http://schemas.microsoft.com/ado/2007/08/dataservices/scheme" term="robblackwell.Tables"/> 
       <content type="application/xml"> 
	<m:properties> 
	 <d:TableName>
	  WADPerformanceCountersTable</d:TableName> 
	</m:properties> 
       </content> 
      </entry> 
      <entry> 
       <id>
	https://robblackwell.table.core.windows.net/Tables('WADWindowsEventLogsTable')</id> 
       <title type="text"/> 
       <updated>
	2011-01-03T16:43:46Z</updated> 
       <author> 
	<name/> 
       </author> 
       <link href="Tables('WADWindowsEventLogsTable')" title="Tables" rel="edit"/> 
       <category scheme="http://schemas.microsoft.com/ado/2007/08/dataservices/scheme" term="robblackwell.Tables"/> 
       <content type="application/xml"> 
	<m:properties> 
	 <d:TableName>
	  WADWindowsEventLogsTable</d:TableName> 
	</m:properties> 
       </content> 
      </entry> 
     </feed>
     T
     CL-AZURE> 

Why?
----

This code was the result of some Christmas holiday hacking inspired by
the [Land of Lisp](http://landoflisp.com/) book, the recent
availability of [Quicklisp](http://www.quicklisp.org/) and Zach
Beane's open source code for Amazon Web Services.

I hope it demonstrates that [Windows
Azure](http://www.microsoft.com/windowsazure/) is an open, cross
platform cloud storage system that isn't tied to Windows or .NET.

If you're not a Lisper, I'd encourage you to look at the above trace a
line at a time, and see just how interactive and incremental the
programmer experience is. Tables, Blobs etc come back as Lists so it
would be easy to slice and dice them. When you want to get under the
covers and see what's going on at the HTTP and XML level, that's easy
too.

It might turn out to be a useful debugging and exploration tool for my
[Windows Azure consultancy work](http://www.aws.net).

Next Steps
----------

I hope to get some time to flesh this out more during 2011 and provide
some better documentation. Any comments, feedback, constructive
criticism or code contributions welcome!

Rob Blackwell

January 2011

