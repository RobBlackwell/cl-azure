;;;; datetime.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

(defconstant +day-names+
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconstant +month-names+
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun rfc1123-date-time-string (&optional (time (get-universal-time)))
  "Returns RFC1123 compliant dates, e.g. Sun, 11 Oct 2009 21:49:13 GMT"
  (multiple-value-bind
	(second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
	    (aref +day-names+ day-of-week)
	    date
	    (aref +month-names+ (1- month))
	    year
	    hour
	    minute
	    second)))

(defun iso8601-date-time-string (&optional (time (get-universal-time)))
  "Returns a ISO8601 compliant dates, e.g. 2011-02-20T14:51:22Z"
  (multiple-value-bind 
	(second minute hour day month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
              year month day hour minute second)))