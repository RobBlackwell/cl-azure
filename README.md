cl-azure
========

Access Microsoft Windows Azure cloud storage from Common Lisp.

Introduction
------------

Windows Azure is Microsoft's cloud hosting platform. This project is
an incomplete, proof-of-concept implementation of a Common Lisp
library for accessing the storage features, namely Blobs, Tables and
Queues.

Instructions
------------

Use Quicklisp to load cl-azure

    > (ql:quickload "cl-azure")

You then need to specify your account details. One way to do this is
to create a LISP file, say myaccount.lisp like this.

     (in-package #:cl-azure)

     (setf *storage-account* (list :account-name "YOUR_ACCOUNT_NAME"
				:account-key "YOUR_ACCOUNT_KEY"
				:table-storage-url 
				"http://YOUR_ACCOUNT_NAME.table.core.windows.net"
				:blob-storage-url 
				"http://YOUR_ACCOUNT_NAME.blob.core.windows.net"
				:queue-storage-url 
				"http://YOUR_ACCOUNT_NAME.queue.core.windows.net"))

From there you're ready to explore

     WA> (list-containers)
     ("$root" "azurerunme" "azurerunme-files" "certs" "drives" "dump" "old"
      "oldpackages" "packages" "printer" "public" "test" "wad-control-container")

     WA> (list-blobs "packages")
     ("AzureRunMe.cspkg" "AzureRunMe.cspkg.sun" "AzureRunMe.cspkg.xxxx"
     "AzureRunMe.cspkg.yyy" "PsTools.zip" "RobBlackwell.cscfg" "adplus.zip"
     "apache-tomcat-6.0.28-windows-x64.zip" "apache-tomcat-7.0.12-windows-x64.zip"
     "clisp-2.49.zip" "dictionarybk" "examples.zip" "jboss-as-web-7.0.0.Final.zip"
     "jdk1.6.0_21.zip" "jdk1.6.0_24.zip" "robblackwell.bat" "runme.zip"
     "telnetd.zip")

     WA> (list-queues)
     ("foo" "wibble" "worker-queue")


If you want to use the Windows Azure Management API, then you'll need a pfx certificate.

You'll need to convert it to PEM format like this:

       % openssl pkcs12 -in your.pfx -out your.pfx.pem

Then you can set it up like this:

     (setf *subscription-id* "YOUR_SUBSCRIPTION_ID")

     (setf *management-certificate* (list
				:certificate "/Users/foo/YOUR.pfx.pem"
				:key "/users/foo/YOUR.pfx.pem"
				:pass-phrase "YOUR_PASSWORD"))

Then you can use it like this:

     WA> (list-hosted-services)
     ("azurerunme" "catalina" "claptrap")	

Experimental support for Service Bus Brokered Messaging:

     WA > (setf *servicebus-credentials* (make-servicebus-credentials "namespace" "owner" "key"))
     ...
     WA > (setf (getf *servicebus-credentials* :token) (get-token)) 
     ....
     WA> (servicebus-send-message "test" "hello world")
     T
     WA> (servicebus-read-and-delete-queue-message "test" )
     "hello world"

Why?
----

This code was the result of some Christmas holiday hacking (2010/2011)
inspired by the [Land of Lisp](http://landoflisp.com/) book, the
recent availability of [Quicklisp](http://www.quicklisp.org/) and Zach
Beane's open source code for Amazon Web Services.

I hope it demonstrates that [Windows
Azure](http://www.microsoft.com/windowsazure/) is an open, cross
platform cloud storage system that isn't tied to Windows or .NET.

It's helped me to understand the Windows Azure API mechanisms, but
more than that, it turns out to be a useful debugging and
experimentation tool.

Any comments, feedback, constructive criticism or code contributions
welcome!

Rob Blackwell

January 2012

