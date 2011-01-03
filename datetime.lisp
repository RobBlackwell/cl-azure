;;;; datetime.lisp
;;;; Copyright (c) 2011, Rob Blackwell.  All rights reserved.

(in-package #:cl-azure)

(defparameter +day-names+
    '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter +month-names+
    '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun rfc1123-date-time-string (&optional (time (get-universal-time)))
  "Returns RFC1123 compliant dates, e.g. Sun, 11 Oct 2009 21:49:13 GMT"
  (multiple-value-bind
	(second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
	    (nth day-of-week +day-names+)
	    date
	    (nth (1- month) +month-names+)
	    year
	    hour
	    minute
	    second)))