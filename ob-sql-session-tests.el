;;; tests.el --- Tests for ob-sql-session.el -*- lexical-binding: t -*-

;;; Code:

(load-file "./ob-sql-session.el")

;; redefine (or patch...)
(defun sql-comint-sqlite (product &optional options buf-name)
  "Create comint buffer and connect to SQLite."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params
         (append options
                 ;; allows connection to in-memory database.
                 (if (and sql-database
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
  (let ((org-babel-sql-session-start-interpreter-prompt
         (lambda (&rest _) t))
        (org-confirm-babel-evaluate
         (lambda (lang body)
           (not (string= lang "sql-session"))))
        )
    (funcall body)))


(defun babel-block-test (setup header code expect)
  "Execute SQL in a `sql-session' Babel block comparing the result against WANT."
  (setup
   (lambda ()
     (let ((buffer-contents (format "
#+begin_src %s
%s
#+end_src" header code)))
       (with-buffer-contents buffer-contents
                             (org-mode)
                             (org-babel-next-src-block)
                             (org-babel-execute-src-block)
                             (should (string= expect (results-block-contents)))
                             )))))

(defun sqlite-test (code expect)
  (babel-block-test #'setup "sql-session :engine sqlite :session Tests" code expect))

(ert-deftest sqllite-000:test-header ()
  "create table."
  (sqlite-test ".headers off" nil))


(ert-deftest sqllite-001:test-create ()
  "create table."
  (sqlite-test "
create table test(one varchar(10), two int);" nil))

(ert-deftest sqllite-002:test-insert ()
  "insert into table."
  (sqlite-test "insert into test values(\'hello\',\'world\');" nil))

(ert-deftest sqllite-003:test-select ()
  "select from table."
  (sqlite-test "select * from test;" "hello|world"))

(ert-deftest sqllite-004:test-tabs ()
  "insert with tabs"
  (sqlite-test "
      --create table test(x,y);
      select * from test;" "hello|world"))

;; gh is on SQLite version 3.37.2 2022-01-06,
;; and its error message is slightly different
;; (ert-deftest sqllite-005:test-stop-on-error ()
;;   "stop on error.
;; joining line isn't ideal on that. May consider solution (2)"
;;   (sqlite-test "create table test(x,y);
;;       select 1;

;; "
;;  "Parse error: table test already exists\n  create table test(x,y);       select 1; \n               ^--- error here" ))

(ert-deftest sqllite-005:test-header-on ()
  (sqlite-test "
.headers on
--create table test(x,y);
delete from test;
insert into test values ('sqlite','3.40');
insert into test values (1,2);
select * from test;"
"one|two
sqlite|3.4
1|2"))

(ert-deftest sqllite-006:test-header-on ()
  (sqlite-test "Drop table test;" nil))

(ert-deftest sqllite-007:test-close-session()
  (with-current-buffer "*SQL: [Tests] sqlite:///nil*"
    (comint-quit-subjob)
    (let ((kill-buffer-query-functions nil))
      (kill-this-buffer))))

;; (eval-buffer)
;; (ert :new)
;; (ert t)
;; (ert-delete-all-tests)

