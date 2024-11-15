;;; ob-sql.el --- Babel Functions for SQL            -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

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
;; - session
;;
;; The following are used but not really implemented for SQL:
;; - colname-names
;; - rownames
;; - rowname-names
;;
;; Engines supported:
;; - mysql/mariadb
;; - sqlite3
;; - dbi
;; - mssql
;; - sqsh
;; - postgresql (postgres)
;; - oracle
;; - vertica
;; - saphana
;;
;; TODO:
;; - support for more engines
;; - provide babel to SQL
;; - expand body for sessions

;;; Code:

(require 'org-macs)
(require 'ob)
(require 'sql)

(defvar ob-sql-session--batch-end-indicator  "---#"  "Indicate the end of a command batch.")
(defvar ob-sql-session-command-terminated nil)
(defvar org-babel-sql-out-file)
(defvar org-babel-sql-session-start-time)

(sql-set-product-feature 'sqlite :prompt-regexp "sqlite> ")
(sql-set-product-feature 'sqlite :batch-terminate
                         (format ".print %s\n" ob-sql-session--batch-end-indicator))
(sql-set-product-feature 'sqlite :terminal-command "\\.")

(sql-set-product-feature 'postgres :prompt-regexp "SQL> ")
(sql-set-product-feature 'postgres :batch-terminate
                         (format "\\echo %s\n" ob-sql-session--batch-end-indicator))
(sql-set-product-feature 'postgres :terminal-command "\\\\")
(sql-set-product-feature 'postgres :environment '(("PGPASSWORD" sql-password)))
(sql-set-product-feature
 'postgres :sqli-options
 (list "--set=ON_ERROR_STOP=1"
       (format "--set=PROMPT1=%s" (sql-get-product-feature 'postgres :prompt-regexp ))
       (format "--set=PROMPT2=%s" (sql-get-product-feature 'postgres :prompt-cont-regexp ))
       "-P" "pager=off"
       "-P" "footer=off"
       "-A" ))

(declare-function org-table-import "org-table" (file arg))
(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function cygwin-convert-file-name-to-windows "cygw32.c" (file &optional absolute-p))
(declare-function sql-set-product "sql" (product))

(defcustom org-babel-default-header-args:sql  '((:engine . "unset"))
  "Default header args."
  :type '(alist :key-type symbol :value-type string
                :options ("dbi" "sqlite" "mysql" "postgres"
                          "sqsh" "mssql" "vertica" "oracle" "saphana" ))
  :group 'org-babel-sql
  :safe t)

(defcustom org-babel-sql-run-comint-p 'nil
  "Run non-session SQL commands through comoint (or command line if nil)."
  :type '(boolean)
  :group 'org-babel-sql
  :safe t)

(defcustom org-babel-sql-timeout '5.0
  "Abort on timeout."
  :type '(number)
  :group 'org-babel-sql
  :safe t)

(defconst org-babel-header-args:sql
  '((engine      . :any)
    (dbhost      . :any)
    (dbport      . :any)
    (dbuser      . :any)
    (dbpassword  . :any)
    (database    . :any)
    (out-file    . :any)
    (dbinstance  . :any))
  "HEADER arguments accepted.")

(defun org-babel-sql-dbstring-mysql (host port user password database)
  "Make MySQL command line arguments for database connection.
Pass nil to omit arguments."
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
  "Make PostgreSQL command line arguments for database connection.
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
connect with <USER>/<PASSWORD>@<HOST>:<PORT>/<DATABASE>
or <user>/<password>@<database> using its alias."

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
  (mapconcat
   #'identity
   (delq nil
         (list (when host (format "-S \"%s\"" (shell-quote-argument host)))
               (when user (format "-U \"%s\"" (shell-quote-argument user)))
               (when password (format "-P \"%s\"" (shell-quote-argument password)))
               (when database (format "-d \"%s\"" (shell-quote-argument database)))))
   " "))

(defun org-babel-sql-dbstring-sqsh (host user password database)
  "Make sqsh command line args for database connection.
sqsh is one method to access Sybase or MS SQL via Linux platform."
  (mapconcat
   #'identity
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
  "Execute a block of SQL code in BODY with PARAMS.
This function is called by `org-babel-execute-src-block'."

  (let* ((result-params (cdr (assq :result-params params)))
         (engine (cdr (assq :engine params)))
         (in-engine  (intern (or engine (user-error "Missing :engine"))))
         (dbhost     (org-babel-find-db-connection-param params :dbhost))
         (dbport     (org-babel-find-db-connection-param params :dbport))
         (dbuser     (org-babel-find-db-connection-param params :dbuser))
         (dbpassword (org-babel-find-db-connection-param params :dbpassword))
         (database   (org-babel-find-db-connection-param params :database))
         (dbinstance (org-babel-find-db-connection-param params :dbinstance))
         (colnames-p (not (equal "no" (cdr (assq :colnames params)))))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assq :out-file params))
                       (org-babel-temp-file "sql-out-")))
         (session (cdr (assoc :session params)))
         (session-p (not (string= session "none")))
         (header-delim ""))

    (setq org-babel-sql-out-file out-file)

    (if (or session-p org-babel-sql-run-comint-p)
        ;; run through comint
        (let ((sql--buffer
               (org-babel-sql-session-connect in-engine params session)))
          (with-current-buffer (get-buffer-create "*ob-sql-result*")
            (erase-buffer))
          (setq org-babel-sql-session-start-time (current-time))
          (setq ob-sql-session-command-terminated nil)

          (with-current-buffer (get-buffer sql--buffer)
            (process-send-string (current-buffer)
                                 (ob-sql-session-format-query
                                  body
                                  ;;(org-babel-expand-body:sql body params)
                                  ))
            ;; todo: check org-babel-comint-async-register
            (while (not ob-sql-session-command-terminated)
              ;; could there be a race condition here as described in (elisp) Accepting Output?
              (sleep-for 0.03))
            ;; command finished, remove filter
            (set-process-filter (get-buffer-process sql--buffer) nil)

            (when (not session-p)
              (comint-quit-subjob)
              ;; despite this quit, the process may not be finished yet
              (let ((kill-buffer-query-functions nil))
                (kill-this-buffer))))

          ;; get results
          (with-current-buffer (get-buffer-create "*ob-sql-result*")
            (goto-char (point-min))
            ;; clear the output or prompt and termination
            (while (re-search-forward
                    (sql-get-product-feature in-engine :ob-sql-session-clean-output)
                    nil t)
              (replace-match ""))
            (write-file out-file)))

      ;; else, command line
      (let* ((cmdline (cdr (assq :cmdline params)))
             (command
              (cl-case in-engine
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
                ((mysql mariadb) (format "mysql %s %s %s < %s > %s"
                                         (org-babel-sql-dbstring-mysql
                                          dbhost dbport dbuser dbpassword database)
                                         (if colnames-p "" "-N")
                                         (or cmdline "")
                                         (org-babel-process-file-name in-file)
                                         (org-babel-process-file-name out-file)))
                ((postgresql postgres) (format
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

        (progn
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
             ;; "sqsh" requires "go" inserted at EOF.
             (if (string= engine "sqsh") "\ngo" "")
             (org-babel-expand-body:sql body params))) ;; insert body
          (org-babel-eval command ""))))

    ;; collect results
    (org-babel-result-cond result-params
      (with-temp-buffer
        (progn (insert-file-contents-literally out-file) (buffer-string)))
      (with-temp-buffer
        (cond
         ((memq in-engine '(dbi sqlite mysql postgresql postgres saphana sqsh vertica))
          ;; Add header row delimiter after column-names header in first line
          (cond
           (colnames-p
            (with-temp-buffer
              (insert-file-contents out-file)
              (goto-char (point-min))
              (forward-line 1)
              (insert "-\n")
              (setq header-delim "-")
              (write-file out-file)))))
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
            (write-file out-file)
            )))

        (when session-p
          (goto-char (point-min))
          ;; clear the output of prompt and termination
          (while (re-search-forward
                  (sql-get-product-feature in-engine :ob-sql-session-clean-output)
                  nil t)
            (replace-match "")))

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

(defun org-babel-edit-prep:sql (info)
  "Prepare Org-edit buffer.
Set `sql-product' in Org edit buffer according to
the :engine header argument provided in INFO."
  (let ((product (cdr (assq :engine (nth 2 info)))))
    (sql-set-product product)))

(defun org-babel-expand-body:sql (body params)
  "Expand BODY according to the values of PARAMS."
  (let ((prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (mapconcat 'identity
               (delq nil (list prologue
                               (org-babel-sql-expand-vars
                                body (org-babel--get-vars params))
                               epilogue))
               "\n")))

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
                                     '(:fmt (lambda (el) (if (stringp el)
                                                        el
                                                      (format "%S" el))))))))
                    data-file)
                (if (stringp val) val (format "%S" val))))
            body t t)))
   vars)
  body)

(defun org-babel-prep-session:sql (_session _params)
  "Raise an error because Sql sessions aren't implemented."
  (message "org-babel-prep-session"))

(defun org-babel-load-session:sql (session body params)
  (message "load session %s" session))

(defun ob-sql-session-buffer-live-p (buffer)
  "Return non-nil if the process associated with buffer is live.

This redefines `sql-buffer-live-p' of sql.el, considering the terminal
is valid even when `sql-interactive-mode' isn't set.  BUFFER can be a buffer
object or a buffer name.  The buffer must be a live buffer, have a
running process attached to it, and, if PRODUCT or CONNECTION are
specified, its `sql-product' or `sql-connection' must match."

  (let ((buffer (get-buffer buffer)))
    (and buffer
         (buffer-live-p buffer)
         (let ((proc (get-buffer-process buffer)))
           (and proc (memq (process-status proc) '(open run)))))))

(defun org-babel-sql-session-connect (in-engine params session)
  "Start the SQL client of IN-ENGINE if it has not.
PARAMS provides the sql connection parameters for a new or
existing SESSION.  Clear the intermediate buffer from previous
output, and set the process filter.  Return the comint process
buffer.

The buffer naming was shortened from
*[session] engine://user@host/database*,
that clearly identifies the connexion from Emacs,
to *SQL [session]* in order to retrieve a session with its
name alone, the other parameters in the header args beeing
no longer needed while the session stays open."
  (sql-set-product in-engine)
  (let* ( (sql-server    (cdr (assoc :dbhost params)))
          ;; (sql-port      (cdr (assoc :port params)))
          (sql-database  (cdr (assoc :database params)))
          (sql-user      (cdr (assoc :dbuser params)))
          (sql-password  (cdr (assoc :dbpassword params)))
          (buffer-name (format "%s" (if (string= session "none") ""
                                      (format "[%s]" session))))
          ;; (buffer-name
          ;;  (format "%s%s://%s%s/%s"
          ;;          (if (string= session "none") "" (format "[%s] " session))
          ;;          engine
          ;;          (if sql-user (concat sql-user "@") "")
          ;;          (if sql-server (concat sql-server ":") "")
          ;;          sql-database))
          (ob-sql-buffer (format "*SQL: %s*" buffer-name)))

    ;; I get a nil on sql-for-each-login on the first call
    ;; to sql-interactive  at
    ;; (if (sql-buffer-live-p ob-sql-buffer)
    ;; so put sql-buffer-live-p aside
    (if (ob-sql-session-buffer-live-p ob-sql-buffer)
        (progn  ; set again the filter
          (set-process-filter (get-buffer-process ob-sql-buffer)
                              #'ob-sql-session-comint-output-filter)
          ob-sql-buffer) ; and return the buffer
      ;; otherwise initiate a new connection
      (save-window-excursion
        (setq ob-sql-buffer              ; start the client
              (ob-sql-connect in-engine buffer-name)))
      (let ((sql-term-proc (get-buffer-process ob-sql-buffer)))
        (unless sql-term-proc
          (user-error (format "SQL %s didn't start" in-engine)))

        ;; clear the welcoming message out of the output from the
        ;; first command, in the case where we forgot quiet mode.
        ;; we can't evaluate how long the connection will take
        ;; so if quiet mode is off and the connexion takes time
        ;; then the welcoming message may show up

        ;;(while (not ob-sql-session-connected))
        ;;(sleep-for 0.10)
        (with-current-buffer (get-buffer ob-sql-buffer) (erase-buffer))
        ;; set the redirection filter
        (set-process-filter sql-term-proc
                            #'ob-sql-session-comint-output-filter)
        ;; return that buffer
        (get-buffer ob-sql-buffer)))))

(defun ob-sql-connect (&optional engine sql-cnx)
  "Run ENGINE interpreter as an inferior process, with SQL-CNX as client buffer.

Imported from sql.el with a few modification in order
to prompt for authentication only if there's a missing
parameter.  Depending on the sql client the password
should also be prompted."

  ;; Get the value of engine that we need
  (setq sql-product
        (cond
         ((assoc engine sql-product-alist) ; Product specified
          engine)
         (t sql-product)))              ; Default to sql-engine

  (when (sql-get-product-feature sql-product :sqli-comint-func)
    ;; If no new name specified or new name in buffer name,
    ;; try to pop to an active SQL interactive for the same engine
    (let (;(buf (sql-find-sqli-buffer sql-product sql-connection)) ; unused yet
          (prompt-regexp (sql-get-product-feature engine :prompt-regexp ))
          (prompt-cont-regexp (sql-get-product-feature engine :prompt-cont-regexp))
          sqli-buffer
          rpt)

      ;; store the regexp used to clear output (prompt1|indicator|prompt2)
      (sql-set-product-feature
       engine :ob-sql-session-clean-output
       (concat "\\(" prompt-regexp "\\)"
               "\\|\\(" ob-sql-session--batch-end-indicator "\n\\)"
               (when prompt-cont-regexp
                 (concat "\\|\\(" prompt-cont-regexp "\\)"))))
      ;; Get credentials.
      ;; either all fields are provided
      ;; or there's a specific case were no login is needed
      ;; or trigger the prompt
      (or (and sql-database sql-user sql-server ) ;sql-port?
          (eq sql-product 'sqlite) ;; sqlite allows in-memory db, w/o login
          (apply #'sql-get-login
                 (sql-get-product-feature engine :sqli-login)))
      ;; depending on client, password is forcefully prompted

      ;; Connect to database.
      ;; (let ((sql-user       (default-value 'sql-user))
      ;;       (sql-password   (default-value 'sql-password))
      ;;       (sql-server     (default-value 'sql-server))
      ;;       (sql-database   (default-value 'sql-database))
      ;;       (sql-port       (default-value 'sql-port))
      ;;       (default-directory (or sql-default-directory default-directory)))

      ;; The password wallet returns a function
      ;; which supplies the password. (untested)
      (when (functionp sql-password)
        (setq sql-password (funcall sql-password)))

      ;; Erase previous sql-buffer as we'll be looking for it's prompt
      ;; to indicate session readyness
      (let ((previous-session
             (get-buffer (format "*SQL: %s*" sql-cnx))))
        (when previous-session
          (with-current-buffer
              previous-session (erase-buffer)))

        (setq sqli-buffer
              (let ((process-environment (copy-sequence process-environment))
                    (variables (sql-get-product-feature engine :environment)))
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
                (secs org-babel-sql-timeout)
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

(defun ob-sql-session-format-query (str)
  "Process then send the command STR to the SQL process.
Provide ENGINE to retrieve product features.
Carefully separate client commands from SQL commands
Concatenate SQL commands as one line is one way to stop on error.
Otherwise the entire batch will be emitted no matter what.
Finnally add the termination command."

  (concat
   (let ((commands (split-string str "\n"))
         (terminal-command
          (concat "^\s*"
                  (sql-get-product-feature sql-product :terminal-command))))
     (mapconcat
      (lambda(s)
        (when (not
               (string-match "\\(^[\s\t]*--.*$\\)\\|\\(^[\s\t]*$\\)" s))
          (concat (replace-regexp-in-string
                   "[\t]" "" ; filter tabs
                   (replace-regexp-in-string "--.*" "" s)) ;; remove comments
                  (when (string-match terminal-command s) "\n"))))
      commands " " )) ; the only way to  stop on error,
   ";\n" (sql-get-product-feature sql-product :batch-terminate) "\n" ))


(defun ob-sql-session-comint-output-filter (_proc string)
  "Process output STRING of PROC gets redirected to a temporary buffer.
It is called several times consecutively as the shell outputs and flush
its message buffer"

  ;; Inserting a result in the sql process buffer (to read it as a
  ;; regular prompt log) inserts it to the terminal, and as a result the
  ;; ouput would get passed as input onto the next command line; See
  ;; `comint-redirect-setup' to possibly fix that,
  ;; (with-current-buffer (process-buffer proc) (insert output))

  (when (or (string-match ob-sql-session--batch-end-indicator string)
            (> (time-to-seconds
                (time-subtract (current-time)
                               org-babel-sql-session-start-time))
               org-babel-sql-timeout))
    (setq ob-sql-session-command-terminated t))

  (with-current-buffer (get-buffer-create "*ob-sql-result*")
    (insert string)))

(provide 'ob-sql)

;;; ob-sql.el ends here
