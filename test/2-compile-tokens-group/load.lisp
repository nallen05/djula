; a test in this test group is run by READ'ing the file "in.sexp", giving the resultant
; sexp to COMPILE-TOKENS in a dynamic environment similar to COMPILE-TEMPLATE [ie,
; *SEEN-VARIABLES* is bound, etc] then simulating the rest of the COMPILE-TEMPLATE /
; render-template process and comparing the results with "out.txt"
;
; if the resultant string is STRING= to the contents of "out.txt" then the test has
; passed
;
; note: trailing and leading whitespace are trimmed before comparison
;
; note: if there is a file "variable-plist.sexp" in the folder then that file is READ
;       and the result bound to the variable *VARIABLE-PLIST*
;
; note: since it's hard to really set up the whole COMPILE-TEMPLATE environment, this
;       test group is best used for simple tests, and complex tests that need the
;       whole dynamic environment should be in the "compile-templates" test group

(in-package :djula-test)

(defun .test-compile-tokens (tokens variable-plist)
  (let ((djula::*template-arguments* variable-plist)
	djula::*known-translation-tables*
	djula::*block-alist*)
    (djula::.funcall-and-concatenate (mapcar 'djula::compile-token tokens))))

(defun !run-compile-tokens-test (test-folder)
  (let ((in.sexp (read-from-string (cl-ffc:slurp-utf-8-file (merge-pathnames "in.sexp" test-folder))))
	(out.txt (cl-ffc:slurp-utf-8-file (merge-pathnames "out.txt" test-folder)))
	(maybe-plist (if #1=(cl-fad:file-exists-p (merge-pathnames "args.sexp" test-folder))
			(read-from-string (cl-ffc:slurp-utf-8-file #1#)))))
    (ptester:test out.txt
		  (.test-compile-tokens in.sexp maybe-plist)
		  :test '.normalized-string=)))