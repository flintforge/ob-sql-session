;;; tests.el --- Tests for ob-sql-session.el -*- lexical-binding: t -*-

;; batch run
;;
;;    ls *.el | entr emacs -batch -l ert -l ob-sql-mode-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(load-file "./ob-sql-session.el")


;; redefine (or patch...)
(defun sql-comint-sqlite (product &optional options buf-name)
  "Create comint buffer and connect to SQLite."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params
         (append options
                 (if (and sql-database ;; allows connection to in-memory database.
													(not (string-empty-p sql-database)))
										 `(,(expand-file-name sql-database))))))
    (sql-comint product params buf-name)))


(defun results-block-contents (&optional position)
  "Return the contents of the *only* results block in the buffer.
Assume the source block is at POSITION if non-nil."
  (interactive)
  (save-excursion
    (progn
      (if position
	  (goto-char position)
		(goto-char 0)
	(org-babel-next-src-block))
      (goto-char (org-babel-where-is-src-block-result))
      (let ((result (org-babel-read-result)))
        result))))

(defmacro with-buffer-contents (s &rest forms)
  "Create a temporary buffer with contents S and execute FORMS."
  `(save-excursion
     (with-temp-buffer
       (progn
	 (goto-char 0)
	 (insert ,s)
	 (goto-char 0)
	 ,@forms))))

(defun setup (body)
  "Initialise the test environment and run BODY."
  ;; (let ((old-sql-get-login (symbol-function 'sql-get-login)))
  ;;   (unwind-protect
	;; 			(progn
					(let ((org-babel-sql-session-start-interpreter-prompt
								 (lambda (&rest _) t))
								(org-confirm-babel-evaluate
								 (lambda (lang body)
									 (not (string= lang "sql-session"))))
																				;(sql-database ob-sql-session-test-database-path)
								)
						;;(defalias 'sql-get-login 'ignore)
						(funcall body)))
			;(defalias 'sql-get-login 'old-sql-get-login))))

(defun babel-block-test (setup header code expect)
  "Execute SQL in a `sql-session' Babel block comparing the result against WANT."
  (setup
   (lambda ()
     (let ((buffer-contents (format "
#+begin_src %s
%s
#+end_src" header code)))
       (with-buffer-contents buffer-contents
			     (org-babel-next-src-block)
			     ;;(org-ctrl-c-ctrl-c)
					 (org-babel-execute-src-block)
			     (should (string= expect (results-block-contents)))							 
		)))))

(defun sqlite-test (code expect)
	(babel-block-test #'setup "sql-session :engine sqlite :session A" code expect))

(ert-deftest sqllite-test-create ()
  "Simple select from no table."
  (sqlite-test "create table test(one varchar(10), two int);" nil))

(ert-deftest sqllite-test-insert ()
  "Simple select from no table."
  (sqlite-test "insert into test values(\'hello\',\'world\');" nil))

(ert-deftest sqllite-test-select ()
  "Simple select from no table."
  (sqlite-test "select * from test;" "hello|world"))
