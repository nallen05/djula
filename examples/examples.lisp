
(defpackage :djula-examples
  (:use :cl)
  (:export :render-base.html
	   :render-hello.html))

(in-package :djula-examples)

(defparameter *system-folder*
  (asdf:component-pathname (asdf:find-component (asdf:find-system 'djula-examples)
						"examples")))

(defparameter  *example-templates-root-folder*
  (merge-pathnames "view-templates/"  *system-folder*))

(djula:def-template render-base.html *example-templates-root-folder* "base.html" ())
(djula:def-template render-hello.html *example-templates-root-folder* "hello.html" ())