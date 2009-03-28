
(defpackage :djula-test
  (:use :cl)
  (:export :!run-tests
	   :!load-tests))

(in-package :djula-test)

; utils used in tests

(defun .trim-whitespace (string)
  (string-trim '(#\space #\tab #\newline #\return) string))

(defun .normalized-string= (string1 string2)
  (string= (.trim-whitespace string1)
	   (.trim-whitespace string2)))

(defun .trim-whitespace/bytes (bytes)
  (trivial-utf-8:string-to-utf-8-bytes
   (string-trim '(#\space #\tab #\newline #\return)
		(trivial-utf-8:utf-8-bytes-to-string bytes))))

(defun .normalized-byte= (bytes1 bytes2)
  (equalp (.trim-whitespace/bytes bytes1)
	  (.trim-whitespace/bytes bytes2)))

; tests

(defparameter *system-folder* (asdf:component-pathname (asdf:find-component (asdf:find-system 'djula-test)
									    "test")))

(portch:def-test-group *system-folder* '!run-tests '!load-tests
		       :run-tests t)