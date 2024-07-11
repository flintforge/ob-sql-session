;;; ob-sql-session.el --- Babel Functions for SQL, with session support -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Phil Estival pe@7d.nz
;; Package-Requires: ((emacs "27.2")) ((org "9.5"))
;; Keywords: literate programming, reproducible research
;; URL: http://github.com/flintforge/ob-sql-session

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating SQL through a terminal interpreter.
;;
;; This replace the previous proposed communication channel set with
;; the comint sql interpreter using `accept-process-output' by a
;; comint output filter through `set-process-filter'.
;; The filter is set during the commands execution and removed on exit.
;; The interactive command interpreter remains usable and
;; `sql-interactive-mode'  can be activated inside.

;; Header args available:
;; - engine
;; - dbhost
;; - database
;; - dbuser
;; - results
;; - session
;; - var

;; Variables declared in a header substitute identifiers prefixed with
;; $ by the associated value.  This does not declare new variables
;; (with a \set command for instance) as they would be stateful and
;; span over blocks in sessions.
;;

;; In session mode, the connexion parameter are required only when
;; login, and no longer required for further requests.

;; SQL commands blocks are passed to the terminal as one unique line
;; while special client commands are given one line at a time.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ob)
(require 'org)
(require 'sql)

(defcustom org-babel-default-header-args:sql-session
  '((:engine . "sqlite"))
  "Default header args."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-babel
  :safe t)

(defconst org-babel-header-args:sql-session
  '((engine     . :any)
    (dbhost     . :any)
    (dbport     . :any)
    (dbuser     . :any)
    (dbpassword . :any)
    (database   . :any)))

;; Batch of SQL commands are terminated by a client command
;; (\echo ---# for instance)
;; While it's possible to hold the command execution while output goes
;; on, we would still need a way to figure out the batch has terminated.
;; Moreover, if a stop on error is set, the command should still be
;; executed as it's not an SQL command to the DB but a string sent to
;; the client terminal.
(defvar ob-sql-session--batch-end-indicator  "---#"  "Indicate the end of a command batch.")
(defvar ob-sql-session-command-terminated nil)

(sql-set-product-feature 'postgres :prompt-regexp "SQL> ")
																				;(sql-set-product-feature 'postgres :prompt-cont-regexp "")
(sql-set-product-feature 'postgres :environment '(("PGPASSWORD" sql-password)))
(sql-set-product-feature 'postgres :batch-terminate
                         (format "\\echo %s\n" ob-sql-session--batch-end-indicator))
(sql-set-product-feature 'postgres :terminal-command "\\\\")

(sql-set-product-feature 'sqlite :prompt-regexp "sqlite> ")
(sql-set-product-feature 'sqlite :batch-terminate
                         (format ".print %s\n" ob-sql-session--batch-end-indicator))
(sql-set-product-feature 'sqlite :terminal-command "\\.")

(setq sql-postgres-options (list
                            "--set=ON_ERROR_STOP=1"
                            (concat "--set=PROMPT1="
                                    (sql-get-product-feature 'postgres :prompt-regexp ))
                            (concat "--set=PROMPT2="
                                    (sql-get-product-feature 'postgres :prompt-cont-regexp ))
                            "-P" "pager=off"
                            "-P" "footer=off"
                            "-A"
                            ))
:

(defun org-babel-execute:sql-session (body params)
  "Execute SQL statements in BODY with PARAMS."
  (let* (
         (processed-params (org-babel-process-params params))
         (session (cdr (assoc :session processed-params)))
         (engine  (cdr (assoc :engine processed-params)))
         (engine  (intern (or engine (user-error "Missing :engine"))))
         (vars (org-babel--get-vars params))
         (results (split-string (cdr (assq :results processed-params ))))
         (session-p (not (string= session "none")))

         (sql--buffer (org-babel-sql-session-connect
                       engine processed-params session)))

    (setq sql-product engine)

    ;; Substitute $vars in body with the associated value. (See also s-format).
    (mapc
     (lambda(v) (setq body (string-replace
                       (concat "$"(symbol-name(car v)))(cdr v) body)))
     vars)

    (with-current-buffer (get-buffer-create "*ob-sql-result*")
      (erase-buffer))

    (setq ob-sql-session-command-terminated nil)
    (with-current-buffer (get-buffer sql--buffer)
      (message "%s" (ob-sql-format-query body engine)) ; debug reformatted commands
      (process-send-string (current-buffer) (ob-sql-format-query body engine))
      ;; todo: check org-babel-comint-async-register
      (while (not ob-sql-session-command-terminated)
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
      (replace-regexp ;; clear the output or prompt and termination
       (sql-get-product-feature engine :ob-sql-session-clear-output) "")

      ;; some client can also directly format to tables
      (when (member "table" results)
        ;;(org-table-convert-region (point-min)(point-max) "|")
      ;;; or
        (goto-char (point-max)) (delete-char -1) ;; last newline
        (beginning-of-line)
        (let ((end (point)))
          (string-insert-rectangle (point-min) end "|"))
        ;; where does this extra | comes from ?
        (goto-char (point-min)) (delete-char 1))

      (buffer-string))))


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
           (and proc (memq (process-status proc) '(open run )))))))


(defun org-babel-sql-session-connect (engine params session)
  "Start the SQL client of ENGINE if it has not in a buffer.
Clear the intermediate buffer from previous output,
and set the process filter.
Return the comint process buffer.

The buffer naming was shortened from '[session]
engine://user@host/database', that clearly identifies the connexion from
Emacs, to *SQL [session]* in order to retrieved a session with its
name alone, the other parameters in the header args beeing no longer needed
once the session is established.

When there is not, the execution will be given to ob-sql.el"


  (let* ((sql-database  (cdr (assoc :database params)))
         (sql-user      (cdr (assoc :dbuser params)))
         (sql-password  (cdr (assoc :dbpassword params)))
         (sql-server    (cdr (assoc :dbhost params)))
         ;; (sql-port (cdr (assoc :port params))) ;; to concat to the server
         (buffer-name (format "%s"(if (string= session "none") ""
																		(format "[%s]" session))))
         ;; (buffer-name (format "%s%s://%s%s/%s"
         ;;                      (if (string= session "none") ""
         ;;                        (format "[%s] " session))
         ;;                      engine
         ;;                      (if sql-user (concat sql-user "@") "")
         ;;                      (if sql-server (concat sql-server ":") "")
         ;;                      sql-database))
         (ob-sql-buffer (format "*SQL: %s*" buffer-name)))

    ;; I get a nil on sql-for-each-login on the first call
    ;; to sql-interactive  at
    ;; (if (sql-buffer-live-p ob-sql-buffer)
    ;; so put sql-buffer-live-p aside

    (if (ob-sql-session-buffer-live-p ob-sql-buffer)
        (progn
          ;; set again the filter
          (set-process-filter (get-buffer-process ob-sql-buffer)
                              #'ob-sql-session-comint-output-filter)
          ;; and return the buffer
          ob-sql-buffer)

      ;; otherwise initiate a connection
      (save-window-excursion
        (setq ob-sql-buffer              ; start the client
              (ob-sql-connect engine buffer-name)))

      (let ((sql-term-proc (get-buffer-process ob-sql-buffer)))
        (unless sql-term-proc
          (user-error (format "SQL %s didn't start" engine)))

        ;; clear the welcoming message out of the output from the
        ;; first command, in the case where we forgot quiet mode.
        ;; we can't evaluate how long the connection will take
        ;; so if quiet mode is off and the connexion takes time
        ;; then the welcoming message may show up

        ;; (set-process-filter sql-term-proc
        ;;                     #'ob-sql-session-comint-connect-filter)
        ;;(while (not ob-sql-session-connected))
        (sleep-for 0.06)
        (with-current-buffer (get-buffer ob-sql-buffer) (erase-buffer))

        ;; set the redirection filter
        (set-process-filter sql-term-proc
                            #'ob-sql-session-comint-output-filter)
        ;; return that buffer
        (get-buffer ob-sql-buffer)))))


(defun ob-sql-connect (&optional engine sql-cnx)
  "Run ENGINE interpreter as an inferior process.

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
    (let ((buf (sql-find-sqli-buffer sql-product sql-connection)) ; unused yet
          (prompt-regexp (sql-get-product-feature engine :prompt-regexp ))
          (prompt-cont-regexp (sql-get-product-feature engine :prompt-cont-regexp))
          sqli-buffer rpt)

      ;; store the regexp used to clear output (prompt1|indicator|prompt2)
      (sql-set-product-feature engine :ob-sql-session-clear-output
															 (concat "\\(" prompt-regexp "\\)"
																			 "\\|\\(" ob-sql-session--batch-end-indicator "\n\\)"
																			 (when prompt-cont-regexp (concat "\\|\\(" prompt-cont-regexp "\\)"))))

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
      (setq rpt (sql-make-progress-reporter nil "Login"))

      (let ((sql-user       (default-value 'sql-user))
            (sql-password   (default-value 'sql-password))
            (sql-server     (default-value 'sql-server))
            (sql-database   (default-value 'sql-database))
            (sql-port       (default-value 'sql-port))
            (default-directory (or sql-default-directory default-directory)))

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
                previous-session (erase-buffer))))

        (setq
         sqli-buffer
         (let ((process-environment (copy-sequence process-environment))
               (variables (sql-get-product-feature engine :environment)))
           (mapc (lambda (elem)   ; environment variables, evaluated here
                   (setenv (car elem) (eval(cadr elem))))
                 variables)
           (funcall (sql-get-product-feature engine :sqli-comint-func)
                    engine
                    (sql-get-product-feature engine :sqli-options)
                    (format "SQL: %s" sql-cnx))))
        ;; no need for a numbered buffer:
        ;; connexion is closed, buffer killed when there's no session
        ;; engine/user/db/session points to the same buffer otherwise
        ;; [2024-06-24 lun.] actually we will need this,
        ;; since the buffer naming changed

        (setq-local sql-buffer (buffer-name sqli-buffer))

        (with-current-buffer sql-buffer
          (let ((proc (get-buffer-process sqli-buffer))
                (secs 3)
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
          (when (not (re-search-backward prompt-regexp 0 t))
            (user-error "Connexion failed")))
        ;;(run-hooks 'sql-login-hook) ; don't
        )
      (sql-progress-reporter-done rpt)
      (get-buffer sqli-buffer))))


(defun ob-sql-format-query (str engine)
  "Process then send the command STR to the SQL process.
Provide ENGINE to retrieve product features.
Carefully separate client commands from SQL commands
Concatenate SQL commands as one line is one way to stop on error.
Otherwise the entire batch will be emitted no matter what.
Finnally add the termination command."

  (setq sql-product engine)
  (concat
   (let ((commands (split-string str "\n"))
         (terminal-command
          (concat "^\s*"
                  (sql-get-product-feature sql-product :terminal-command))))
     (mapconcat
      (lambda(s)
        (when (not
               (string-match "\\(^[\s\t]*--.*$\\)\\|\\(^[\s\t]*$\\)" s))
          (concat (replace-regexp-in-string "[\t]" "" ; filter tabs
																						(replace-regexp-in-string "--.*" "" s)) ;; remove comments
                  (when (string-match terminal-command s) "\n"))))
      commands " " )) ; the only way to  stop on error,
   "\n" (sql-get-product-feature sql-product :batch-terminate) "\n" ))


(defun ob-sql-session-comint-output-filter (_proc string)
  "Process output STRING of PROC gets redirected to a temporary buffer.
It is called several times consecutively as the shell outputs and flush
its message buffer"

  ;; Inserting the result in the sql process buffer
  ;; adds it to the terminal prompt and as a result
  ;; the ouput gets passed as input onto the next command
  ;; line; See `comint-redirect-setup' to possibly fix that
  ;; (with-current-buffer (process-buffer proc) (insert output))

  (when (string-match ob-sql-session--batch-end-indicator string)
    (setq ob-sql-session-command-terminated t))

  (with-current-buffer (get-buffer-create "*ob-sql-result*")
    (insert string)))

(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("sql-session" . sql))
  (add-to-list 'org-babel-tangle-lang-exts '("sql-session" . "sql")))
;;(add-to-list 'org-structure-template-alist '("sql" . "src sql-session-mode")
;; or (customize-variable 'org-structure-template-alist)


(provide 'ob-sql-session)

;; LocalWords: sql org-mode session
;;; ob-sql-session.el ends here
