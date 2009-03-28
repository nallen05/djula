
(defpackage :djula
  (:use :cl :logv :f-underscore)
  (:export ; lisp api
           :compile-template

	   ; runtime options
	   :*language*
	   :*catch-template-errors-p*
	   :*run-rulebook-tests-p*
	   :*allow-include-roots*
	   :*template-string-if-invalid*
	   :*devel-only-p*

	   ; compile-time option
	   :*template-folder*
	   
	   ; filter documentation
	   :get-all-filters
	   :get-filter-documentation

	   ; tag documentation
	   :get-all-tags
	   :get-tag-documentation

	   ; building html documentation
	   :build-html-documentation-from-source

	   ; used by cl-terrace
	   :dictionary-p
	   :compile-dictionary

	   :devel-dictionary-p
	   :compile-devel-dictionary))