
(in-package :djula)

; util

(defun .documentation->alist (documentation)
  "turns the stuff returned by GET-FILTER-DOCUMENTATION and GET-TAG-DOCUMENTATION into
 an alist for the Djula Server Index Page template"
  (labels ((plist->alist (plist)
	     (when plist
	       (destructuring-bind (k v . rest) plist
		 (cons (cons k v) (plist->alist rest))))))
    (plist->alist (destructuring-bind (name arglist doc-string . plist) documentation
		    (list* :name name
			   :arglist arglist
			   :documentation doc-string
			   plist)))))

; filters used in the view only index page template

(def-filter :link-tags-to-definitions (string)
  (cl-ppcre:regex-replace-all "{% (\\S+)([^}]*)}" string "<a href='#tag-\\1' class='djula-tag'>{% \\1\\2}<\a>"))

(def-filter :link-variables-to-documentation (string)
  (cl-ppcre:regex-replace-all "{{([^}]*)}}" string "<a href='#variables' class='djula-tag'>{{\\1}}<\a>"))

(def-filter :link-comments-to-documentation (string)
  (cl-ppcre:regex-replace-all "{#([^}]*)#}" string "<a href='#comments' class='djula-tag'>{#\\1#}<\a>"))

(def-filter :link-dictionary-variables-to-documentation (string)
  (cl-ppcre:regex-replace-all "{_([^}]*)_}" string "<a href='#dictionary-variables' class='djula-tag'>{_\\1_}<\a>"))

; doit

(defun build-html-documentation-from-source (&key output-file)
  (if output-file
      (with-open-file (out output-file
			   :element-type '(unsigned-byte 8)
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :error)
	(trivial-utf-8:write-utf-8-bytes (build-html-documentation-from-source)
					 out)
	output-file)
      (funcall (compile-template *djula-documentation-template-source-folder*
				 *djula-template-api-documentation-template*)
	       :tags (mapcar '.documentation->alist
			     (mapcar 'djula:get-tag-documentation
				     (djula:get-all-tags)))
	       :filters (mapcar '.documentation->alist
				(mapcar 'djula:get-filter-documentation
					(djula:get-all-filters))))))
