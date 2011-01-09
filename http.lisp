;;;; http.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

(defconstant +http-continue+ 100 "Continue")
(defconstant +http-switching-protocols+ 101 "Switching Protocols")
(defconstant +http-ok+ 200 "OK")
(defconstant +http-created+ 201 "Created")
(defconstant +http-accepted+ 202 "Accepted")
(defconstant +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(defconstant +http-no-content+ 204 "No Content")
(defconstant +http-reset-content+ 205 "Reset Content")
(defconstant +http-partial-content+ 206 "Partial Content")
(defconstant +http-multi-status+ 207 "Multi-Status")
(defconstant +http-multiple-choices+ 300 "Multiple Choices")
(defconstant +http-moved-permanently+ 301 "Moved Permanently")
(defconstant +http-moved-temporarily+ 302 "Found")
(defconstant +http-see-other+ 303 "See Other")
(defconstant +http-not-modified+ 304 "Not Modified")
(defconstant +http-use-proxy+ 305 "Use Proxy")
(defconstant +http-temporary-redirect+ 307 "Temporary Redirect")
(defconstant +http-bad-request+ 400 "Bad Request")
(defconstant +http-authorization-required+ 401 "Unauthorized")
(defconstant +http-payment-required+ 402  "Payment Required")
(defconstant +http-forbidden+ 403 "Forbidden")
(defconstant +http-not-found+ 404 "Not Found")
(defconstant +http-method-not-allowed+ 405 "Method Not Allowed")
(defconstant +http-not-acceptable+ 406 "Not Acceptable")
(defconstant +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(defconstant +http-request-time-out+ 408 "Request Timeout")
(defconstant +http-conflict+ 409 "Conflict")
(defconstant +http-gone+ 410 "Gone")
(defconstant +http-length-required+ 411 "Length Required")
(defconstant +http-precondition-failed+ 412 "Precondition Failed")
(defconstant +http-request-entity-too-large+ 413 "Request Entity Too Large")
(defconstant +http-request-uri-too-large+ 414 "Request-URI Too Large")
(defconstant +http-unsupported-media-type+ 415 "Unsupported Media Type")
(defconstant +http-requested-range-not-satisfiable+ 416 "Requested Range Not Satisfiable")
(defconstant +http-expectation-failed+ 417 "Expectation Failed")
(defconstant +http-failed-dependency+ 424 "Failed Dependency")
(defconstant +http-internal-server-error+ 500 "Internal Server Error")
(defconstant +http-not-implemented+ 501 "Not Implemented")
(defconstant +http-bad-gateway+ 502 "Bad Gateway")
(defconstant +http-service-unavailable+ 503 "Service Unavailable")
(defconstant +http-gateway-time-out+ 504 "Gateway Timeout")
(defconstant +http-version-not-supported+ 505 "HTTP Version Not Supported")