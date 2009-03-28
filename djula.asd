;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:djula-system
  (:use :cl))

(in-package #:djula-system)

(asdf:defsystem :djula
  :depends-on (:cl-ppcre
	       :cl-ffc
	       :cl-fad
	       :trivial-utf-8
	       :logv
	       :split-sequence
	       :f-underscore)
  :description "spork of django templating engine for julia"
  :version "0.1"
  :author "Nick Allen <nallen05@gmail.com"
  :components
  ((:module :src
	    :components ((:file "package")
			 (:file "util")
			 (:file "core")
			 (:file "table")
			 (:file "variable")
			 (:file "filter-definitions")
			 (:file "tag")
			 (:file "tag-definitions")
			 (:file "tag-documentation")
			 (:file "build-documentation"))
	    :serial t)))