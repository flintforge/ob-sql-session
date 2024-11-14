;;; tests.el --- Tests for ob-sql-session.el -*- lexical-binding: t -*-

;;; Code:

(load-file "./ob-sql.el")

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
           (not (or (string= lang "sql")
                    (string= lang "sql-session"))))))
    (funcall body)))


(defun babel-block-test (setup header code expect &optional expected-result)
  "Execute SQL in a `sql-session' Babel block comparing the result against WANT."
  (setup
   (lambda ()
     (let ((buffer-contents (format "
#+begin_src %s
%s
#+end_src" header code)))
       (with-buffer-contents
        buffer-contents
        (org-mode)
        (org-babel-next-src-block)
        (org-babel-execute-src-block)
        (should (string= expect (results-block-contents)))
        )))))

(defun sqlite-test (code expect &optional expected-result)
  (babel-block-test #'setup
                    "sql :engine sqlite :session Tests :results raw"
                    code expect))

(ert-deftest sqllite-000:test-header ()
  "Create table."
  (sqlite-test ".headers off" nil))


(ert-deftest sqllite-001:test-create ()
  "Create table."
  (sqlite-test ".headers off

create table test(one varchar(10), two int);" nil))

(ert-deftest sqllite-002:test-insert ()
  "Insert into table."
  (sqlite-test "insert into test values(\'hello\',\'world\');" nil))

(ert-deftest sqllite-003:test-select ()
  "Select from table."
  (sqlite-test "select * from test;"
               "hello|world\n"))

(ert-deftest sqllite-004:test-filter-tabs ()
  "Insert with tabs."
  (sqlite-test "
      --create table test(x,y);
        select * from test;

"
               "hello|world\n"))

;; gh is on SQLite version 3.37.2 2022-01-06,
;; and its error message is slightly different
;; (ert-deftest sqllite-005:test-stop-on-error ()
;;   "stop on error.
;; joining line isn't ideal on that. May consider solution (2)"
;;   (sqlite-test "create table test(x,y);
;;       select 1;

;; "
;;  "Parse error: table test already exists\n  create table test(x,y);       select 1; \n               ^--- error here" ))

(ert-deftest sqllite-005a:test-multiple-commands ()
  "Copy pasting this in sqlite3 will give the same result."
  :expected-result :failed
  (sqlite-test
   "
    .headers on
-- ?
    .bail on

select 1;
"
   "Parse error: near \".\": syntax error\n  .headers on       .bail on " ;  select 1; \n  ^--- error here"
   )) ;; variations expected between sqlite versions

(ert-deftest sqllite-005a:test-commands ()
  (sqlite-test
   ".headers on
" nil))

(ert-deftest sqllite-005b:test-header-on ()
  (sqlite-test
   ".headers on
--create table test(x,y);
    delete from test;
    insert into test values ('sqlite','3.40');
    insert into test values (1,2);
    select * from test;"

   "one|two
sqlite|3.4
1|2
"))

;; additionally, an error after a command can clutter the next shell
;;

(ert-deftest sqllite-006:drop ()
  (sqlite-test "Drop table test;" nil))

(ert-deftest sqllite-007:test-close-session()
  (with-current-buffer "*SQL: [Tests]*" ; sqlite:///nil*"
    (quit-process nil t)
    (let ((kill-buffer-query-functions nil))
      (kill-this-buffer))))

(defun pg-test (code expect &optional expected-result)
  (babel-block-test
   #'setup
   "sql :session PG::tests :engine postgres :dbhost localhost :database pg :dbuser pg :dbpassword pg :results raw"
   code expect))

(ert-deftest pg-000:test-session-var ()
  "Select in a table."
  (pg-test "\\set id10 10 \n \\set id13 13" nil))

(ert-deftest pg-001:test-session-var-read ()
  "Select in a table."
  (pg-test "select :id10 as A,:id13 as B;" "a|b\n10|13\n"))

(ert-deftest pg-002:test-session-drop ()
  "insert in a table."
  (pg-test "DROP TABLE IF EXISTS publications;" "DROP TABLE\n"))

(ert-deftest pg-003:test-session-test-create-table ()
  "insert in a table."
  (pg-test "CREATE TABLE publications (id int2, database text);" "CREATE TABLE\n"))

(ert-deftest pg-004:test-session-insert ()
  "insert in a table."
  (pg-test "insert into publications values (:id10, 'HGNC'), (:id13, 'FlyBase');" "INSERT 0 2\n"))

(ert-deftest pg-005:test-session-query ()
  "Select in a table."
  (pg-test "SELECT database from publications where id=:id10 or id=:id13;"
           "database\nHGNC\nFlyBase\n"))


;; (eval-buffer)
;; (ert :new)
;; (ert t)
;; (ert-delete-all-tests)
;; (with-current-buffer "ob-sql.el" (save-buffer))
;; (progn (ert-delete-all-tests)(eval-buffer)(ert :new))
