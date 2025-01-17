;;; ob-sql.el --- Babel Functions for SQL            -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Daniel Kraus <daniel@kraus.my>
;; Maintainer: Philippe Estival <pe@7d.nz>
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating sql source code.
;; (see also ob-sqlite.el)
;;
;; SQL is somewhat unique in that there are many different engines for
;; the evaluation of sql (Mysql, PostgreSQL, etc...), so much of this
;; file will have to be implemented engine by engine.
;;
;; Also SQL evaluation generally takes place inside of a database.
;;
;; Header args used:
;; - engine
;; - cmdline
;; - session
;; - dbhost
;; - dbport
;; - dbuser
;; - dbpassword
;; - dbconnection (to reference connections in sql-connection-alist)
;; - dbinstance (currently only used by SAP HANA)
;; - database
;; - colnames (default, nil, means "yes")
;; - result-params
;; - out-file
;;
;; The following are used but not really implemented for SQL:
;; - colname-names
;; - rownames
;; - rowname-names
;;
;; Engines supported:
;; - mysql/mariadb
;; - dbi
;; - mssql
;; - sqsh
;; - postgresql (postgres)
;; - sqlite
;; - oracle
;; - vertica
;; - saphana
;;
;; Limitation:
;; - sessions:
;;   - engines configured: sqlite, postgres
;;   - no error line number (stays as LINE 1)
;;   - default port number only
;;
;; TODO:
;;
;; - support for more engines
;; - babel tables as input
;; - raw replace result
;; - port number configuration for sessions
;;

;;; Code:

(require 'ob)
(require 'sql)

(defvar org-babel-sql-session-start-time)
(defvar org-sql-session-preamble
  (list
   'postgres "\\set ON_ERROR_STOP 1
\\pset footer off
\\pset pager off
\\pset format unaligned"	 )
  "Command preamble to run upon shell start.")
(defvar org-sql-session-command-terminated nil)
(defvar org-sql-session--batch-terminate  "---#"  "To print at the end of a command batch.")
(defvar org-sql-batch-terminate
  (list 'sqlite (format ".print %s\n" org-sql-session--batch-terminate)
        'postgres (format "\\echo %s\n" org-sql-session--batch-terminate))
  "Print the command batch termination as last command.")
(defvar org-sql-terminal-command-prefix
  (list 'sqlite "\\."
        'postgres "\\\\")
  "Identify a command for the SQL shell.")
(defvar org-sql-environment
  (list 'postgres '(("PGPASSWORD" sql-password))))
(defvar org-sql-session-clean-output nil
  "Store the regexp used to clear output (prompt1|termination|prompt2).")
(defvar org-sql-session-start-time)
(defvar org-sql-session-command-terminated nil)
(defvar org-sql-session--batch-terminate  "---#"  "To print at the end of a command batch.")

(declare-function org-table-import "org-table" (file arg))
(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function cygwin-convert-file-name-to-windows "cygw32.c" (file &optional absolute-p))
(declare-function sql-set-product "sql" (product))

(defvar sql-connection-alist)
(defcustom org-babel-default-header-args:sql  '((:engine . "unset"))
  "Default header args."
  :type '(alist :key-type symbol :value-type string
                :options ("dbi" "sqlite" "mysql" "postgres"
                          "sqsh" "mssql" "vertica" "oracle" "saphana" ))
  :group 'org-babel-sql
  :safe t)

(defcustom org-sql-run-comint-p 'nil
  "Run non-session SQL commands through comint if not nil."
  :type '(boolean)
  :group 'org-babel-sql
  :safe t)

(defcustom org-sql-timeout '5.0
  "Abort on timeout."
  :type '(number)
  :group 'org-babel-sql
  :safe t)

(defcustom org-sql-close-out-temp-buffer-p 'nil
  "To automatically close sql-out-temp buffer."
  :type '(boolean)
  :group 'org-babel-sql
  :safe t)

(defconst org-babel-header-args:sql
  '((engine	       . :any)
    (out-file	       . :any)
    (dbhost	       . :any)
    (dbport	       . :any)
    (dbuser	       . :any)
    (dbpassword	       . :any)
    (dbinstance	       . :any)
    (database	       . :any))
  "SQL-specific header arguments.")

(defun org-babel-expand-body:sql (body params)
  "Expand BODY according to the values of PARAMS."
  (let ((prologue (cdr (assq :prologue params)))
				(epilogue (cdr (assq :epilogue params))))
    (mapconcat 'identity
							 (delq nil (list
													prologue
													(org-babel-sql-expand-vars
													 body (org-babel--get-vars params))
													epilogue))
               "\n")))

(defun org-babel-edit-prep:sql (info)
  "Set `sql-product' in Org edit buffer.
Set `sql-product' in Org edit buffer according to the
corresponding :engine source block header argument."
  (let ((product (cdr (assq :engine (nth 2 info)))))
    (sql-set-product product)))

(defun org-babel-sql-dbstring-mysql (host port user password database)
  "Make MySQL cmd line args for database connection.  Pass nil to omit that arg."
  (mapconcat
   #'identity
   (delq nil
				 (list (when host     (concat "-h" (shell-quote-argument host)))
							 (when port     (format "-P%d" port))
							 (when user     (concat "-u" (shell-quote-argument user)))
							 (when password (concat "-p" (shell-quote-argument password)))
							 (when database (concat "-D" (shell-quote-argument database)))))
   " "))

(defun org-babel-sql-dbstring-postgresql (host port user database)
  "Make PostgreSQL command line args for database connection.
Pass nil to omit that arg."
  (mapconcat
   #'identity
   (delq nil
				 (list (when host (concat "-h" (shell-quote-argument host)))
							 (when port (format "-p%d" port))
							 (when user (concat "-U" (shell-quote-argument user)))
							 (when database (concat "-d" (shell-quote-argument database)))))
   " "))

(defun org-babel-sql-dbstring-oracle (host port user password database)
  "Make Oracle command line arguments for database connection.

If HOST and PORT are nil then don't pass them.  This allows you
to use names defined in your \"TNSNAMES\" file.  So you can
connect with

  <user>/<password>@<host>:<port>/<database>

or

  <user>/<password>@<database>

using its alias."
  (when user (setq user (shell-quote-argument user)))
  (when password (setq password (shell-quote-argument password)))
  (when database (setq database (shell-quote-argument database)))
  (when host (setq host (shell-quote-argument host)))
  (cond ((and user password database host port)
				 (format "%s/%s@%s:%d/%s" user password host port database))
				((and user password database)
				 (format "%s/%s@%s" user password database))
				(t (user-error "Missing information to connect to database"))))

(defun org-babel-sql-dbstring-mssql (host user password database)
  "Make sqlcmd command line args for database connection.
`sqlcmd' is the preferred command line tool to access Microsoft
SQL Server on Windows and Linux platform."
  (mapconcat #'identity
						 (delq nil
									 (list (when host (format "-S \"%s\"" (shell-quote-argument host)))
												 (when user (format "-U \"%s\"" (shell-quote-argument user)))
												 (when password (format "-P \"%s\"" (shell-quote-argument password)))
												 (when database (format "-d \"%s\"" (shell-quote-argument database)))))
						 " "))

(defun org-babel-sql-dbstring-sqsh (host user password database)
  "Make sqsh command line args for database connection.
\"sqsh\" is one method to access Sybase or MS SQL via Linux platform"
  (mapconcat #'identity
             (delq nil
                   (list  (when host     (format "-S \"%s\"" (shell-quote-argument host)))
                          (when user     (format "-U \"%s\"" (shell-quote-argument user)))
                          (when password (format "-P \"%s\"" (shell-quote-argument password)))
                          (when database (format "-D \"%s\"" (shell-quote-argument database)))))
             " "))

(defun org-babel-sql-dbstring-vertica (host port user password database)
  "Make Vertica command line args for database connection.
Pass nil to omit that arg."
  (mapconcat #'identity
						 (delq nil
									 (list (when host     (format "-h %s" (shell-quote-argument host)))
												 (when port     (format "-p %d" port))
												 (when user     (format "-U %s" (shell-quote-argument user)))
												 (when password (format "-w %s" (shell-quote-argument password) ))
												 (when database (format "-d %s" (shell-quote-argument database)))))
						 " "))

(defun org-babel-sql-dbstring-saphana (host port instance user password database)
  "Make SAP HANA command line args for database connection.
Pass nil to omit that arg."
  (mapconcat #'identity
             (delq nil
                   (list (and host port (format "-n %s:%s"
                                                (shell-quote-argument host)
                                                port))
                         (and host (not port) (format "-n %s" (shell-quote-argument host)))
                         (and instance (format "-i %d" instance))
                         (and user (format "-u %s" (shell-quote-argument user)))
                         (and password (format "-p %s"
                                               (shell-quote-argument password)))
                         (and database (format "-d %s" (shell-quote-argument database)))))
             " "))

(defun org-babel-sql-convert-standard-filename (file)
  "Convert FILE to OS standard file name.
If in Cygwin environment, uses Cygwin specific function to
convert the file name.  In a Windows-NT environment, do nothing.
Otherwise, use Emacs's standard conversion function."
  (cond ((fboundp 'cygwin-convert-file-name-to-windows)
				 (format "%S" (cygwin-convert-file-name-to-windows file)))
				((string= "windows-nt" system-type) file)
				(t (format "%S" (convert-standard-filename file)))))

(defun org-babel-find-db-connection-param (params name)
  "Return database connection parameter NAME.
Given a parameter NAME, if :dbconnection is defined in PARAMS
then look for the parameter into the corresponding connection
defined in `sql-connection-alist', otherwise look into PARAMS.
See `sql-connection-alist' (part of SQL mode) for how to define
database connections."
  (or (cdr (assq name params))
      (and (assq :dbconnection params)
           (let* ((dbconnection (cdr (assq :dbconnection params)))
                  (name-mapping '((:dbhost . sql-server)
                                  (:dbport . sql-port)
                                  (:dbuser . sql-user)
                                  (:dbpassword . sql-password)
                                  (:dbinstance . sql-dbinstance)
                                  (:database . sql-database)))
                  (mapped-name (cdr (assq name name-mapping))))
             (cadr (assq mapped-name
                         (cdr (assoc-string dbconnection sql-connection-alist t))))))))

(defun org-babel-execute:sql (body params)
  "Execute SQL BODY with PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assq :result-params params)))
         (cmdline (cdr (assq :cmdline params)))
         (dbhost     (org-babel-find-db-connection-param params :dbhost))
         (dbport     (org-babel-find-db-connection-param params :dbport))
         (dbuser     (org-babel-find-db-connection-param params :dbuser))
         (dbpassword (org-babel-find-db-connection-param params :dbpassword))
         (dbinstance (org-babel-find-db-connection-param params :dbinstance))
         (database   (org-babel-find-db-connection-param params :database))
         (engine (cdr (assq :engine params)))
         (in-engine  (intern (or engine (user-error "Missing :engine"))))
         (colnames-p (not (equal "no" (cdr (assq :colnames params)))))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assq :out-file params))
                       (org-babel-temp-file "sql-out-")))
				 (header-delim "")
         (session (cdr (assoc :session params)))
         (session-p (not (string= session "none"))))

    (if (or session-p org-sql-run-comint-p) ; run through comint
        (let ((sql--buffer
							 (org-babel-sql-session-connect in-engine params session)))
					(with-current-buffer (get-buffer-create "*ob-sql-result*")
						(erase-buffer))
					(setq org-sql-session-start-time (current-time))
					(setq org-sql-session-command-terminated nil)

					(with-current-buffer (get-buffer sql--buffer)
            ;;(message "%s" (org-babel-expand-body:sql body params))
						(process-send-string (current-buffer)
																 (org-sql-session-format-query
                                  (org-babel-expand-body:sql body params)
                                  in-engine))
						(while (or (not org-sql-session-command-terminated)
                       (> (time-to-seconds
                           (time-subtract (current-time)
                                          org-sql-session-start-time))
                          org-sql-timeout))
							(sleep-for 0.03))
						;; command finished, remove filter
						(set-process-filter (get-buffer-process sql--buffer) nil)

						(when (not session-p)
							(comint-quit-subjob)
							;; despite this quit signal, the process may not be finished yet
							(let ((kill-buffer-query-functions nil))
								(kill-this-buffer))))

					(with-current-buffer (get-buffer "*ob-sql-result*")
						(goto-char (point-min))
						;; clear the output or prompt and termination
						(let ((clean-output (plist-get org-sql-session-clean-output in-engine)))
							(while (re-search-forward clean-output nil t)
								(replace-match "")))
						(write-file out-file)))

      (let ( ; else run a shell command
						(command (cl-case in-engine
											 (dbi (format "dbish --batch %s < %s | sed '%s' > %s"
																		(or cmdline "")
																		(org-babel-process-file-name in-file)
																		"/^+/d;s/^|//;s/(NULL)/ /g;$d"
																		(org-babel-process-file-name out-file)))
											 (sqlite (format "sqlite3 < %s > %s"
																			 (org-babel-process-file-name in-file)
																			 (org-babel-process-file-name out-file)))
											 (monetdb (format "mclient -f tab %s < %s > %s"
																				(or cmdline "")
																				(org-babel-process-file-name in-file)
																				(org-babel-process-file-name out-file)))
											 (mssql (format "sqlcmd %s -s \"\t\" %s -i %s -o %s"
																			(or cmdline "")
																			(org-babel-sql-dbstring-mssql
																			 dbhost dbuser dbpassword database)
																			(org-babel-sql-convert-standard-filename
																			 (org-babel-process-file-name in-file))
																			(org-babel-sql-convert-standard-filename
																			 (org-babel-process-file-name out-file))))
											 (mysql (format "mysql %s %s %s < %s > %s"
																			(org-babel-sql-dbstring-mysql
																			 dbhost dbport dbuser dbpassword database)
																			(if colnames-p "" "-N")
																			(or cmdline "")
																			(org-babel-process-file-name in-file)
																			(org-babel-process-file-name out-file)))
											 ((postgresql postgres)
												(format
												 "%s%s --set=\"ON_ERROR_STOP=1\" %s -A -P \
footer=off -F \"\t\"  %s -f %s -o %s %s"
												 (if dbpassword
														 (format "PGPASSWORD=%s "
																		 (shell-quote-argument dbpassword))
													 "")
												 (or (bound-and-true-p
															sql-postgres-program)
														 "psql")
												 (if colnames-p "" "-t")
												 (org-babel-sql-dbstring-postgresql
													dbhost dbport dbuser database)
												 (org-babel-process-file-name in-file)
												 (org-babel-process-file-name out-file)
												 (or cmdline "")))
											 (sqsh (format "sqsh %s %s -i %s -o %s -m csv"
																		 (or cmdline "")
																		 (org-babel-sql-dbstring-sqsh
																			dbhost dbuser dbpassword database)
																		 (org-babel-sql-convert-standard-filename
																			(org-babel-process-file-name in-file))
																		 (org-babel-sql-convert-standard-filename
																			(org-babel-process-file-name out-file))))
											 (vertica (format "vsql %s -f %s -o %s %s"
																				(org-babel-sql-dbstring-vertica
																				 dbhost dbport dbuser dbpassword database)
																				(org-babel-process-file-name in-file)
																				(org-babel-process-file-name out-file)
																				(or cmdline "")))
											 (oracle (format
																"sqlplus -s %s < %s > %s"
																(org-babel-sql-dbstring-oracle
																 dbhost dbport dbuser dbpassword database)
																(org-babel-process-file-name in-file)
																(org-babel-process-file-name out-file)))
											 (saphana (format "hdbsql %s -I %s -o %s %s"
																				(org-babel-sql-dbstring-saphana
																				 dbhost dbport dbinstance dbuser dbpassword database)
																				(org-babel-process-file-name in-file)
																				(org-babel-process-file-name out-file)
																				(or cmdline "")))
											 (t (user-error "No support for the %s SQL engine" engine)))))
				(with-temp-file in-file
					(insert
					 (pcase in-engine
						 (`dbi "/format partbox\n")
						 (`oracle "SET PAGESIZE 50000
SET NEWPAGE 0
SET TAB OFF
SET SPACE 0
SET LINESIZE 9999
SET TRIMOUT ON TRIMSPOOL ON
SET ECHO OFF
SET FEEDBACK OFF
SET VERIFY OFF
SET HEADING ON
SET MARKUP HTML OFF SPOOL OFF
SET COLSEP '|'

")
						 ((or `mssql `sqsh) "SET NOCOUNT ON

")
						 (`vertica "\\a\n")
						 (_ ""))
					 (org-babel-expand-body:sql body params)
					 ;; "sqsh" requires "go" inserted at EOF.
					 (if (string= engine "sqsh") "\ngo" "")))
				(org-babel-eval command "")))
    (org-babel-result-cond result-params
      (with-temp-buffer
				(progn (insert-file-contents-literally out-file) (buffer-string)))
      (with-temp-buffer
				(cond
				 ((memq in-engine '(dbi mysql postgresql postgres saphana sqsh vertica))
					;; Add header row delimiter after column-names header in first line
					(when colnames-p (with-temp-buffer
														 (insert-file-contents out-file)
														 (goto-char (point-min))
														 (forward-line 1)
														 (insert "-\n")
														 (setq header-delim "-")
														 (write-file out-file))))
				 (t
					;; Need to figure out the delimiter for the header row
					(with-temp-buffer
						(insert-file-contents out-file)
						(goto-char (point-min))
						(when (re-search-forward "^\\(-+\\)[^-]" nil t)
							(setq header-delim (match-string-no-properties 1)))
						(goto-char (point-max))
						(forward-char -1)
						(while (looking-at "\n")
							(delete-char 1)
							(goto-char (point-max))
							(forward-char -1))
						(write-file out-file))))
				(org-table-import out-file (if (string= engine "sqsh") '(4) '(16)))
				(org-babel-reassemble-table
				 (mapcar (lambda (x)
									 (if (string= (car x) header-delim)
											 'hline
										 x))
								 (org-table-to-lisp))
				 (org-babel-pick-name (cdr (assq :colname-names params))
															(cdr (assq :colnames params)))
				 (org-babel-pick-name (cdr (assq :rowname-names params))
															(cdr (assq :rownames params))))))))

(defun org-babel-sql-expand-vars (body vars &optional sqlite)
  "Expand the variables held in VARS in BODY.

If SQLITE has been provided, prevent passing a format to
`orgtbl-to-csv'.  This prevents overriding the default format, which if
there were commas in the context of the table broke the table as an
argument mechanism."
  (mapc
   (lambda (pair)
     (setq body
					 (replace-regexp-in-string
						(format "$%s" (car pair))
						(let ((val (cdr pair)))
              (if (listp val)
                  (let ((data-file (org-babel-temp-file "sql-data-")))
                    (with-temp-file data-file
                      (insert (orgtbl-to-csv
                               val (if sqlite
                                       nil
                                     '( :fmt (lambda (el)
                                               (if (stringp el)
                                                   el
                                                 (format "%S" el)))
                                        :with-special-rows nil)))))
                    data-file)
                (if (stringp val) val (format "%S" val))))
						body t t)))
   vars)
  body)

(defun org-babel-prep-session:sql (_session _params)
  "Raise an error because Sql sessions aren't implemented."
  (error "SQL sessions not yet implemented"))

(defun org-babel-sql-session-connect (in-engine params session)
  "Start the SQL client of IN-ENGINE if it has not.
PARAMS provides the sql connection parameters for a new or
existing SESSION.  Clear the intermediate buffer from previous
output, and set the process filter.  Return the comint process
buffer."
  (let* ((buffer-name (format "%s" (if (string= session "none") ""
                                     (format "[%s]" session))))
         (ob-sql-buffer (format "*SQL: %s*" buffer-name)))

    ;; initiate a new connection
    (when (not (org-babel-comint-buffer-livep ob-sql-buffer))
      (save-window-excursion
        (setq ob-sql-buffer  ; start the client
              (org-babel-sql-connect in-engine buffer-name params)))
      (let ((sql-term-proc (get-buffer-process ob-sql-buffer)))
        (unless sql-term-proc
          (user-error (format "SQL %s didn't start" in-engine)))

        (with-current-buffer (get-buffer ob-sql-buffer)
          ;; preamble commands
          (let ((preamble (plist-get org-sql-session-preamble in-engine)))
            (when preamble
              (process-send-string ob-sql-buffer preamble)
              (comint-send-input))))
        ;; let the preamble execution finish and be filtered
        (sleep-for 0.1)))

    ;; set the redirection filter and return the SQL client buffer
    (set-process-filter (get-buffer-process ob-sql-buffer)
                        #'org-sql-session-comint-output-filter)
    (get-buffer ob-sql-buffer)))

(defun org-babel-sql-connect (&optional engine sql-cnx params)
  "Run ENGINE interpreter as an inferior process.
SQL-CNX is the client buffer.  This is a variant from sql.el that prompt
parametrs for authentication only if there's a missing parameter.
Depending on the sql client the password should also be prompted."

  (setq sql-product(cond
                    ((assoc engine sql-product-alist) ; Product specified
                     engine)
                    (t sql-product))) ; or default to sql-engine

  (when (sql-get-product-feature sql-product :sqli-comint-func)
    (let (;(buf (sql-find-sqli-buffer sql-product sql-connection)) ; unused yet
          (sql-server    (cdr (assoc :dbhost params)))
          ;; (sql-port      (cdr (assoc :port params))) ; todo
          (sql-database  (cdr (assoc :database params)))
          (sql-user      (cdr (assoc :dbuser params)))
          (sql-password  (cdr (assoc :dbpassword params)))
          (prompt-regexp (sql-get-product-feature engine :prompt-regexp ))
          (prompt-cont-regexp (sql-get-product-feature engine :prompt-cont-regexp))
          sqli-buffer
          rpt)
      ;; store the regexp used to clear output (prompt1|indicator|prompt2)
      (setq org-sql-session-clean-output
            (plist-put org-sql-session-clean-output engine
                       (concat "\\(" prompt-regexp "\\)"
                               "\\|\\(" org-sql-session--batch-terminate "\n\\)"
                               (when prompt-cont-regexp
                                 (concat "\\|\\(" prompt-cont-regexp "\\)")))))
      ;; Get credentials.
      ;; either all fields are provided
      ;; or there's a specific case were no login is needed
      ;; or trigger the prompt
      (or (and sql-database sql-user sql-server)
          (eq sql-product 'sqlite) ;; sqlite allows in-memory db, w/o login
          (apply #'sql-get-login
                 (sql-get-product-feature engine :sqli-login)))
      ;; depending on client, password is forcefully prompted

      ;; The password wallet returns a function
      ;; which supplies the password. (untested)
      (when (functionp sql-password)
        (setq sql-password (funcall sql-password)))

      ;; Erase previous sql-buffer.
      ;; Will look for it's prompt to indicate session readyness.
      (let ((previous-session
             (get-buffer (format "*SQL: %s*" sql-cnx))))
        (when previous-session
          (with-current-buffer
              previous-session (erase-buffer)))

        (setq sqli-buffer
              (let ((process-environment (copy-sequence process-environment))
                    (variables (plist-get org-sql-environment engine)))
                (mapc (lambda (elem)   ; environment variables, evaluated here
                        (setenv (car elem) (eval (cadr elem))))
                      variables)
                (funcall (sql-get-product-feature engine :sqli-comint-func)
                         engine
                         (sql-get-product-feature engine :sqli-options)
                         (format "SQL: %s" sql-cnx))))
        (setq sql-buffer (buffer-name sqli-buffer))

        (setq rpt (sql-make-progress-reporter nil "Login"))
        (with-current-buffer sql-buffer
          (let ((proc (get-buffer-process sqli-buffer))
                (secs org-sql-timeout)
                (step 0.2))
            (while (and proc
                        (memq (process-status proc) '(open run))
                        (or (accept-process-output proc step)
                            (<= 0.0 (setq secs (- secs step))))
                        (progn (goto-char (point-max))
                               (not (re-search-backward
                                     prompt-regexp 0 t))))
              (sql-progress-reporter-update rpt)))

          ;; no prompt, connexion failed (and process is terminated)
          (goto-char (point-max))
          (unless (re-search-backward prompt-regexp 0 t)
            (user-error "Connection failed"))) ;is this a _user_ error?
        ;;(run-hooks 'sql-login-hook) ; don't
        )
      (sql-progress-reporter-done rpt)
      (get-buffer sqli-buffer))))

(defun org-sql-session-format-query (str in-engine)
  "Process then send the command STR to the SQL process.
Provide IN-ENGINE to retrieve product features.
Carefully separate client commands from SQL commands
Concatenate SQL commands as one line is one way to stop on error.
Otherwise the entire batch will be emitted no matter what.
Finnally add the termination command."
  (concat
   (let ((commands (split-string str "\n"))
         (terminal-command
          (concat "^\s*"
                  (plist-get org-sql-terminal-command-prefix in-engine))))
     (mapconcat
      (lambda(s)
        (when (not
               (string-match "\\(^[\s\t]*--.*$\\)\\|\\(^[\s\t]*$\\)" s))
          (concat (replace-regexp-in-string
                   "[\t]" "" ; filter tabs
                   (replace-regexp-in-string "--.*" "" s)) ;; remove comments.
                  ;; Note: additional filtering is required for Vertica C-style comments.
                  (when (string-match terminal-command s) "\n"))))
      commands " " ))
   ";\n"
   (plist-get org-sql-batch-terminate in-engine)
   "\n" ))

(defun org-sql-session-comint-output-filter (_proc string)
  "Process output STRING of PROC gets redirected to a temporary buffer.
It is called several times consecutively as the shell outputs and flush
its message buffer"

  ;; Inserting a result in the sql process buffer (to read it as a
  ;; regular prompt log) inserts it to the terminal, and as a result the
  ;; ouput would get passed as input onto the next command line; See
  ;; `comint-redirect-setup' to possibly fix that,
  ;; (with-current-buffer (process-buffer proc) (insert output))

  (when (or (string-match org-sql-session--batch-terminate string)
            (> (time-to-seconds
                (time-subtract (current-time)
                               org-sql-session-start-time))
               org-sql-timeout))
    (setq org-sql-session-command-terminated t))

  (with-current-buffer (get-buffer-create "*ob-sql-result*")
    (insert string)))

(provide 'ob-sql)

;;; ob-sql.el ends here
