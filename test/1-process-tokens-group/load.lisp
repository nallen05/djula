; a test in this test group is run by READ'ing the files "in.sexp" and "out.sexp"
; and calling PROCESS-TOKENS with the result of READ'ing "in.sexp". If the value
; returned by PROCESS-TOKENS is EQUALP to the result of READ'ing "out.sexp" then the
; test passes

(in-package :djula-test)

(defun !run-process-tokens-test (test-folder)
  (let ((in.sexp (read-from-string (cl-ffc:slurp-utf-8-file (merge-pathnames "in.sexp" test-folder))))
	(out.sexp (read-from-string (cl-ffc:slurp-utf-8-file (merge-pathnames "out.sexp" test-folder)))))
    (ptester:test out.sexp
		  (djula::process-tokens in.sexp)
		  :test 'equalp)))