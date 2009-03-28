;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:djula-test-system
  (:use :cl))

(in-package #:djula-test-system)

(asdf:defsystem :djula-test
  :depends-on (:djula :portch :trivial-utf-8)
  :description "tests for djula templating system"
  :components
  ((:module :test
	    :components ((:file djula-test)))))