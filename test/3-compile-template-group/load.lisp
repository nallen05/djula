; a test in this test group is run by compiling the file "in.txt" as a template
; then comparing the output of the resulting function with the file "out.txt".
;
; if they are STRING= when the function returns a string and EQUALP when the function
; returns bytes then the test passes.
;
; if there is also a file "args.sexp" in the folder, then that file is READ and the
; result used as the variable-plist argument to the function returned by
; COMPILE-TEMPLATE [note: the contents of "args.sexp" are _not_ EVAL'd]
;
; if there is also a file "bindings.sexp" in the folder, then that file is READ and the
; result assumed to be an alist of special variable bindings that will be in effect
; during the test [note: the contents of "bindings.sexp" _are_ EVAL'd]
;
; note: whitespace is trimmed from the front and end of the string/bytes before being
;       tested for STRING= or EQUALP equality


(in-package :djula-test)

(defun .get-special-binding-alist (test-folder)
  (let ((p (cl-fad:file-exists-p (merge-pathnames "bindings.sexp" test-folder))))
    (if p
	(eval (read-from-string (format nil "(list ~A)" (cl-ffc:slurp-utf-8-file p)))))))

(defun .special-binding-alist->progv-args (alist)
  (values (mapcar 'first alist)
	  (mapcar 'second alist)))

(defun .render-test-template (named test-folder &key return-bytes)
  (multiple-value-bind (vars vals)
      (.special-binding-alist->progv-args (.get-special-binding-alist test-folder))
    (progv vars vals
      (apply (djula:compile-template test-folder
				     (merge-pathnames named test-folder)
				     :return-format (if return-bytes
							:octets
							:string))
	     (let ((args.sexp (cl-fad:file-exists-p (merge-pathnames "args.sexp" test-folder))))
	       (if args.sexp
		   (read-from-string (cl-ffc:slurp-utf-8-file args.sexp))))))))

(defun .run-compile-template-test/string (test-folder)
  (let ((out.txt (cl-ffc:slurp-utf-8-file (merge-pathnames "out.txt" test-folder))))
    (when (ptester:test out.txt
			(.render-test-template "in.txt" test-folder)
			:test '.normalized-string=)
      t)))

(defun .run-compile-template-test/bytes (test-folder)
  (let ((out.txt/bytes (trivial-utf-8:string-to-utf-8-bytes (cl-ffc:slurp-utf-8-file (merge-pathnames "out.txt" test-folder)))))
    (when (ptester:test out.txt/bytes
			(.render-test-template "in.txt" test-folder :return-bytes t)
			:test '.normalized-byte=)
      t)))

(defun !run-compile-template-test (test-folder)
  (and (.run-compile-template-test/string test-folder)
       (.run-compile-template-test/bytes test-folder)))