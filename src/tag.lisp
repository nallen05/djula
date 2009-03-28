; this file extends PROCESS-TOKENS and COMPILE-TOKEN to understand :UNPARSED-TAG
; and :TAG tokens
;
; see "tag-definitions.lisp" for the source code of the individual tags and
; "tag-documentation.lisp" for the documentation of the tags

(in-package :djula)

; processing unparsed tags

(defun .semi-parse-tag (string)
  (let ((*package* (find-package :keyword))
	(*read-eval* nil))
    (read-from-string string)))

(defun .parse-rest-of-tag (string start)               ;; this is big a kludge!!! fix later
  (let ((*package* (find-package :keyword))
	(*read-eval* nil))
    (with-template-error (template-error-string "there was an error parsing the tag ~A" string)
      (values (read-from-string (format nil "(~A)" (subseq string start)))))))

(def-token-processor :unparsed-tag (unparsed-string) rest
  (multiple-value-bind (tag-name start-rest) (.semi-parse-tag unparsed-string)
    (let ((f (get tag-name 'unparsed-tag-processor)))
      (if f
	  (funcall f rest (subseq unparsed-string start-rest))
	  (multiple-value-bind (ret error-string)
	      (.parse-rest-of-tag unparsed-string
				  start-rest)
	    (process-tokens `(,(if error-string
				   `(:string ,error-string)
				   `(:tag ,tag-name ,@ret))
			       ,@rest)))))))

; defining tag processors

(defmacro def-unparsed-tag-processor (name args rest-var &body body)
  `(setf (get ,name 'unparsed-tag-processor)
	 (m (,rest-var ,@args) ,@body)))

(defmacro def-tag-processor (name args rest-var &body body)
  `(setf (get ,name 'tag-processor)
	 (m (,rest-var ,@args) ,@body)))

; processing tags

(def-token-processor :tag (name . args) rest
  ":TAG tokens are sometimes parsed into some other tokens by PROCESS-TOKENS"
  (let ((f (get name 'tag-processor)))
    (if (null f)
	`((:tag ,name ,@args) ,@(process-tokens rest))
	(with-template-error `((:string ,(template-error-string "There was an error processing tag ~A" name))
			       ,@(process-tokens rest))
	  (apply f rest args)))))

; util for tags with start and end tag

(defun .find-end-tag (tag-name tokens)
  "returns NIL if a :TAG token with the name `TAG-NAME' can't be found in `TOKENS'.
Otherwise returns three values:

   1. a list of all the tokens up to that token
   2. a list of all tokens after that token
   3. T, indicating that `TAG-NAME' was found"
  (let ((n (position-if (f_ (destructuring-bind (token-name . args) _
			      (and (eql token-name :tag)
				   (eql (first args) tag-name))))
			tokens)))
    (if n
	(values (subseq tokens 0 n)
		(subseq tokens (1+ n))
		t))))

(defmacro def-delimited-tag (starttag endtag delimited-name)
  `(progn

     (def-tag-processor ,starttag (&rest args) unprocessed
       (with-template-error `((:string ,(template-error-string "There was an error processing the delmited tag {% ~A %} {% ~A %}" ,starttag ,endtag)))
	 (multiple-value-bind (processed-clause processed-rest present-p)
	     (.find-end-tag ,endtag (process-tokens unprocessed))
	   (if (not present-p)
	       `((:string ,(template-error-string "Error parsing delimited tag {% ~A %}, cannot find closing tag {% ~A %}" ,starttag ,endtag)))
	       (process-tokens `((,,delimited-name ,args ,@processed-clause) ,@processed-rest))))))

     (def-tag-compiler ,endtag (&optional argument)
       (constantly (template-error-string "unmatched ending tag {% ~A~A %}"
					  ,endtag
					  (if argument
					      (format nil " ~A" argument))
					  "")))))

; defining tag compilers

(defmacro def-tag-compiler (name args &body body)
  `(setf (get ,name 'tag-compiler)
	 (m ,args ,@body)))

; compiling tags

(def-token-compiler :tag (name . args)
  (let ((f (get name 'tag-compiler)))
    (if (null f)
	(constantly (template-error-string "Unknown tag ~A" name))
	(with-template-error (constantly (template-error-string "There was an error compiling tag ~A" name))
	  (apply f args)))))

; documenting tags

(defun def-tag-documentation (name arglist doc-string &key from-django-p but-different-from-django-because)
  (setf (get name 'tag-documentation)
	(list arglist
	      doc-string
	      :from-django-p
	      from-django-p
	      :but-different-from-django-because
	      but-different-from-django-because)))

; getting tag documentation

(defun get-tag-documentation (keyword)
  (if #1=(get keyword 'tag-documentation)
      (cons keyword #1#)))

; getting a list of all tags

(defun get-all-tags ()
  (let (tags)
    (do-symbols (k :keyword (sort tags 'string< :key 'string))
      (if (get-tag-documentation k)
	(push k tags)))))