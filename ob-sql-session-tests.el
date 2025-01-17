;;; tests.el --- Tests for ob-sql-session.el -*- lexical-binding: t -*-

;;; TODO: verify files sql-in and -out-*
;;; tests are according to the the lexicographic order of the function names
;;; Code:

(load-file "./ob-sql.el")

(require 'org)
(require 'sql)
(require 'ert)
(org-version nil t t)
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

(defun babel-block-test (setup header code expect)
  "Given SETUP function, execute SQL CODE block with HEADER.
Compare the result against EXPECT."
  (setup
   (lambda ()
     (let ((buffer-contents (format "
#+begin_src %s
%s
#+end_src" header code)))
       (message buffer-contents)
       (with-buffer-contents
        buffer-contents
        (org-mode)
        (org-babel-next-src-block)
        (org-babel-execute-src-block)
        (message "=>\n%s" (results-block-contents))
        (should (equal expect (results-block-contents)))
        )))))

(defun sqlite-test (code expect &optional header-args)
  "Test CODE block with optional HEADER-ARGS, EXPECT ing this result."
  (babel-block-test
   #'setup (concat "sql :engine sqlite :results raw " header-args)
   code expect))

(defun sqlite-test-table (code expect &optional header-args)
  "Test CODE block with optional HEADER-ARGS, EXPECT ing this result."
  (babel-block-test
   #'setup (concat "sql :engine sqlite :results table output " header-args)
   code expect))

(ert-deftest 000-sqlite:test-select ()
  "Basic select."
  (sqlite-test "select 1" "1\n" ))

(ert-deftest 000-sqlite:test-session-select ()
  "Basic select."
  (sqlite-test "select 1,2" "1|2\n" ":session sqlite::tests" ))

(ert-deftest 000-sqlite:test-select-table ()
  "Select table."
  (sqlite-test-table ".headers on
 select 1 as a, 2 as b;" '(("a" "b") (1 2)) nil))

(ert-deftest 000-sqlite:test-header ()
  "Hedaers off."
  (sqlite-test ".headers off" nil))


(ert-deftest 001-sqlite:test-create ()
  "Header off, Create table."
  (sqlite-test ".headers off

create table test(one varchar(10), two int);"
							 nil
							 ":session sqlite::tests"
							 ))

(ert-deftest 002-sqlite:test-insert ()
  "Insert into table."
  (sqlite-test "insert into test values(\'hello\',\'world\');"
							 nil
							 ":session sqlite::tests"))

(ert-deftest 003-sqlite:test-select ()
  "Select from table."
  (sqlite-test "select * from test;"
               "hello|world\n"
							 ":session sqlite::tests"))

(ert-deftest 004-sqlite:test-filter-tabs ()
  "Insert with tabs."
  (sqlite-test "
			--create table test(x,y);
				select * from test;

"
               "hello|world\n"
							 ":session sqlite::tests"
							 ))

;; gh is on SQLite version 3.37.2 2022-01-06,
;; and its error message is slightly different
;; (ert-deftest sqlite-005:test-stop-on-error ()
;;   "stop on error.
;; joining line isn't ideal on that. May consider solution (2)"
;;   (sqlite-test "create table test(x,y);
;;       select 1;

;; "
;;  "Parse error: table test already exists\n  create table test(x,y);       select 1; \n               ^--- error here" ))

;; (ert-deftest sqlite-005a:test-multiple-commands ()
;;   "Copy pasting this in sqlite3 will give the same result."
;;   :expected-result :failed
;;   (sqlite-test
;;    "
;;     .headers on
;; -- ?
;;     .bail on

;; select 1;
;; "
;;    "Parse error: near \".\": syntax error\n  .headers on       .bail on " ;  select 1; \n  ^--- error here"
;;    )) ;; variations expected between sqlite versions

(ert-deftest 005a-sqlite:test-commands ()
  (sqlite-test
   ".headers on
" nil))

(ert-deftest 005b-sqlite:test-header-on ()
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
"							 ":session sqlite::tests"))

;; additionally, an error after a command can clutter the next shell
;;

(defun pg-test (code expect)
  "Test Postgres sql CODE, with EXPECT ed result."
  (babel-block-test
   #'setup
   "sql :engine postgres :dbhost localhost :database pg :dbuser pg :dbpassword pg :results raw :var var=33"
   code expect))

(defun pg-test-session (code expect)
  "Test Postgres SQL CODE in a session, with EXPECT ed result."
  (babel-block-test
   #'setup
   "sql :engine postgres :dbhost localhost :database pg :dbuser pg :dbpassword pg :results raw \
:session pg::tests"
   code expect))

(ert-deftest 006-sqlite:drop ()
  (sqlite-test "Drop table test;" nil))

(ert-deftest X-sqlite:test-close-session()
  (with-current-buffer "*SQL: [sqlite::tests]*" ; sqlite:///nil*"
    (quit-process nil t)
    (let ((kill-buffer-query-functions nil))
      (kill-this-buffer))))

(ert-deftest 001-pg:test-session-var-set ()
  "Select in a table."
  (pg-test-session "\\set id10 10 \n \\set id13 13" nil))

(ert-deftest 002-pg:test-session-var-read ()
  "Select in a table."
  (pg-test-session "select :id10 as A,:id13 as B;" "a|b\n10|13\n"))

(ert-deftest 003-pg:test-create-insert-select ()
  "Select in a table."
  (pg-test "DROP TABLE if exists publications;
CREATE TABLE publications (id int2, database text);
INSERT INTO publications VALUES (10, 'HGNC'), (13, 'FlyBase');
SELECT database FROM publications where id=10 or id=13;" "DROP TABLE
CREATE TABLE
INSERT 0 2
database\nHGNC\nFlyBase\n"))

(ert-deftest 004-pg:test-expand-variable ()
  "Expand variable."
  (pg-test "select $var as var;" "var\n33\n"))

(ert-deftest X-pg:test-close-session()
  (with-current-buffer "*SQL: [pg::tests]*" ; sqlite:///nil*"
    (quit-process nil t)
    (let ((kill-buffer-query-functions nil))
      (kill-this-buffer))))

;; (kill-emacs)
;; (ert t)
;; (ert-delete-all-tests)
;; (with-current-buffer "ob-sql.el" (save-buffer))
;; (progn (ert-delete-all-tests)(eval-buffer)(ert :new))
;; (progn (eval-buffer) (ert :new))
