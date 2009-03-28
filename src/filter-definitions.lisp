
(in-package :djula)

; capfirst

(def-filter-documentation :capfirst '(thing)
  "Capitalizes the first character of the value"
  :from-django-p t)

(def-filter :capfirst (it)
  (string-capitalize (princ-to-string it)))

; cup

(def-filter-documentation :cut '(thing)
"Removes all values of arg from the given string.

For example:

   {{ value|cut:\" \"}}

If value is \"String with spaces\", the output will be \"Stringwithspaces\"."
:from-django-p t)

(def-filter :cut (it charstring)
  (remove-if (f_ (find _ charstring :test 'char=))
	     (princ-to-string it)))  

; default

(def-filter-documentation :default '(thing default)
"...document me..."
:from-django-p t
:but-different-from-django-because
"I'm not connected to the internet so I can't read the django documentation")

(def-filter :default (it default)
  (if (zerop (length it))
      default
      it))

; force_escape

(def-filter-documentation :force_escape '(thing)
"Applies HTML escaping to a string (see the escape filter for details). This filter is
applied immediately and returns a new, escaped string. This is useful in the rare cases
where you need multiple escaping or want to apply other filters to the escaped results.
Normally, you want to use the escape filter."
  :from-django-p t)

(def-filter :force_escape (it)
  (djula-html-escape (princ-to-string it)))

; length

(def-filter-documentation :length '(thing)
"Returns the length of the value. This works for both strings and lists.

For example:

   {{ value|length }}

If value is \"abcd\" the output will be 4."
  :from-django-p t)

(def-filter :length (it)
  (length (princ-to-string it)))

; linebreaks

(def-filter-documentation :linebreaks '(thing)
"Replaces line breaks in plain text with appropriate HTML; a single newline becomes an
HTML line break (<br />) and a new line followed by a blank line becomes a paragraph
break (</p>).

For example:

   {{ value|linebreaks }}

If value is Joel\\nis a slug, the output will be <p>Joe<br>is a slug</p>."
  :from-django-p t)

(def-filter :linebreaks (it)
  (cl-ppcre:regex-replace-all "\\n"
			      (cl-ppcre:regex-replace-all "(.+)((\\n\\n)|$)"
							  (princ-to-string it)
							  "<p>\\1</p>")
			      "<br />"))

; linebreaksbr

(def-filter-documentation :linebreaksbr '(thing)
"Converts all newlines in a piece of plain text to HTML line breaks (<br />)."
:from-django-p t)

(def-filter :linebreaksbr (it)
  (cl-ppcre:regex-replace-all "\\n"
			      (princ-to-string it)
			      "<br />"))

; lisp

(def-filter-documentation :lisp '(thing lisp-string)
"takes a unary function and applies it to `THING'. Assumes the \"cl-user\" package. So
if there is a variable that has the value \"The Foo\" then

   {{foo|lisp:(lambda (x) (concatenate 'string x \" and \" x))}}

shows up in the browser as


   \"The Foo and The Foo\"

and

   {{foo|lisp:reverse}}

shows up in the browser as

   \"ooF ehT\"

note: lisp filters can be turned off by setting the variable *EVAL-LISP-TAGS* to NIL
")

(def-filter :lisp (it &optional lisp-string)
  (if (not *eval-lisp-tags*)
      (with-template-error #1=(template-error-string "I can't evaulate the \"lisp\" filter ~A because *EVAL-LISP-TAGS* is NIL"
						     lisp-string)
			   (error #1#))
      (with-template-error (template-error-string "There was an error reading the lisp tag ~S"
						  lisp-string)
	(let* ((*package* (find-package :cl-user))
	       (sexp (read-from-string lisp-string)))
	  (with-template-error (template-error-string "There was an error interpreting the lisp tag ~S"
						      sexp)
	    (let ((fn (coerce sexp 'function)))
	      (with-template-error (template-error-string "There was an error executing the lisp tag ~S"
							  sexp)
		(funcall fn it))))))))

; lower

(def-filter-documentation :lower '(thing)
"Converts a string into all lowercase. For example:

   {{ value|lower }}

If value is \"Still MAD At Yoko\", the output will be \"still mad at yoko\"."
  :from-django-p t)

(def-filter :lower (it)
  (string-downcase (princ-to-string it)))

; safe

(def-filter-documentation :safe '(thing)
"Marks a string as not requiring further HTML escaping prior to output."
:from-django-p t)

(def-filter :safe (it)
   ;; CAVEAT: THIS FILTER IS TREATED SPECIALLY BY COMPILE-TOKEN!!!
   it)

; truncatechars

(def-filter-documentation :truncatechars '(thing )
"Truncates a string after a certain number of characters.

Argument: Number of characters to truncate after

For example:

   {{ value|truncatechars:9 }}

If value is \"Joel is a slug\", the output will be \"Joel is a...\".")

(def-filter :truncatechars (it n)
  (let ((n (if (stringp n)
	       (parse-integer n :junk-allowed t)
	       n))
	(string (princ-to-string it)))
    (if (> (length string) n)
	(concatenate 'string (subseq string 0 n) "...")
	string)))

; upper

(def-filter-documentation :upper '(thing)
"Converts a string into all uppercase.

For example:

   {{ value|upper }}

If value is \"Joel is a slug\", the output will be \"JOEL IS A SLUG\""
  :from-django-p t)

(def-filter :upper (it)
  (string-upcase (princ-to-string it)))

; urlencode

(def-filter-documentation :urlencode '(thing)
"Escapes a value for use in a URL."
  :from-django-p t)

(def-filter :urlencode (it)
  (djula-url-encode (princ-to-string it)))