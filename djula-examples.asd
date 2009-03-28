;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:djula-examples-system
  (:use :cl))

(in-package #:djula-examples-system)

(asdf:defsystem :djula-examples
  :depends-on (:djula)
  :description "examples for djula templating system"
  :components
  ((:module :examples
	    :components ((:file examples)))))