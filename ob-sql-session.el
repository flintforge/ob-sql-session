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
;; The interactive command interpreter remains usable.

;; Header args available:
;; - engine
;; - dbhost
;; - database
;; - dbuser
;; - results
;; - session
;; - var

;; Variables declared in a header substitute identifiers prefixed with
;; $ by the associated value. This does not declare new variables
;; (with a \set command for instance) as they would be stateful and
;; span over blocks in sessions.

;; Not implemented
;; - dbconnection
;; - dbpassword : will be prompted on the first connection
;;
;; Commands blocks are passed to the terminal as one unique line

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ob)
(require 'org)
(require 'sql)


(defcustom org-babel-default-header-args:sql-session
  '((:engine . "sqlite"))
  "Default header args."
  :group 'org-babel
  :safe t)

(defconst org-babel-header-args:sql-session
  '((engine     . :any)
    (dbhost     . :any)
    (dbport     . :any)
    (dbuser     . :any)
    ;;(dbpassword . :any)
    (database   . :any)))


;; Batch of SQL commands are terminated by a client command
;; (\echo -----; for instance)
;; It's possible to hold the command execution while output goes on.
;; But we need a way to figure out the batch has terminated
;; However, if a stop on error is set, the command should still be
;; executed as it's not an SQL command to the DB but a command to the
;; client terminal.
(defvar ob-sql-session--batch-end-indicator  "---#"  "Indicate the end of a command batch")

(sql-set-product-feature 'postgres :prompt-regexp "SQL> ")
(sql-set-product-feature 'postgres :prompt-cont-regexp "")
(sql-set-product-feature 'postgres :batch-terminate
                         (format "\\echo %s\n" ob-sql-session--batch-end-indicator))

;;(sql-set-product-feature 'sqlite :prompt-regexp "sqlite> ")

;; continuation prompt can appear on the same line. why?
;; remove ^ . from regex
(sql-set-product-feature 'sqlite :prompt-regexp "sqlite> ")
(sql-set-product-feature 'sqlite :prompt-cont-regexp "   \\.\\.\\.> ")
(sql-set-product-feature 'sqlite :batch-terminate
                         (format ".print %s\n" ob-sql-session--batch-end-indicator))

(setq sql-postgres-options (list
                            "--set=ON_ERROR_STOP=1"
                            (concat "--set=PROMPT1="
                                    (sql-get-product-feature 'postgres :prompt-regexp ))
                            (concat "--set=PROMPT2="
                                    (sql-get-product-feature 'postgres :prompt-cont-regexp ))
                            ;;"-q" ;; quiet mode would also suppress CREATE FUNCTION
                            "-P" "pager=off"
                            "-P" "footer=off"
                            "-A"
                            ;;"--tuples-only"
                            ;; an option to switch it internatly (\pset tuples_only)
                            ;; would be intersting, but
                            ))


(defun org-babel-execute:sql-session (body params)
  "Execute SQL statements in BODY using PARAMS."

  (let* (
         (processed-params (org-babel-process-params params))
         (session (cdr (assoc :session processed-params)))
         (engine  (cdr (assoc :engine processed-params)))
         (engine  (if (not engine) (user-error "missing :engine") (intern engine)))
         (vars (org-babel--get-vars params))
         (results (split-string (cdr (assq :results processed-params ))))
         (session-p (not (string= session "none")))

         (sql--buffer (org-babel-sql-session-connect
                       engine processed-params session session-p))
         )

    ;; Substitute $vars in body with the associated value.
    (mapcar
     (lambda(v) (setq body (string-replace
                       (concat "$"(symbol-name(car v)))(cdr v) body)))
     vars)

    (with-current-buffer (get-buffer-create "*ob-sql-result*")
      (erase-buffer))

    (setq ob-sql-session-command-terminated nil)
    (with-current-buffer (get-buffer sql--buffer)
      (ob-sql-send-string body (current-buffer))
      ;; check org-babel-comint-async-register
      (while (not ob-sql-session-command-terminated)
        (sleep-for 0.03))
      ;; command finished, remove filter
      (set-process-filter (get-buffer-process sql--buffer) nil)

      (when (not session-p)
        (comint-quit-subjob)
        ;; despite this quit, the process may not be finished
        (let ((kill-buffer-query-functions nil))
          (kill-this-buffer)))
      )
    ;; get results
    (with-current-buffer (get-buffer-create "*ob-sql-result*")

      (goto-char (point-min))
      (replace-regexp ;; clear the output or prompt and termination
       (sql-get-product-feature engine :ob-sql-session-clear-output) "")
      ;;(message "replace %s" (sql-get-product-feature engine :ob-sql-session-clear-output))

      ;; some client can also directly format to tables
      (when (member "table" results)
        ;;(org-table-convert-region (point-min)(point-max) "|")
      ;;; or
        (goto-char (point-max)) (backward-delete-char 1) ;; last newline
        (beginning-of-line)
        (let ((end (point)))
          (string-insert-rectangle (point-min) end "|"))
        ;; where does this extra | comes from ?
        (goto-char (point-min)) (delete-char 1))

      (buffer-string)
      )))


(defun org-babel-sql-session-connect (engine params session session-p)
  "Starts the SQL client if it has not in a buffer
named *SQL: [engine]:[user@server:/database]*
clear the intermediate buffer from previous output,
and set the process filter.
Return the comint process buffer."
  (unless engine
    (user-error (format "missing :engine parameter")))

  (let* ((sql-database  (cdr (assoc :database params)))
         (sql-user      (cdr (assoc :dbuser params)))
         (sql-password  (cdr (assoc :dbpassword params)))
         (sql-server    (cdr (assoc :dbhost params)))
         ;; (sql-port (cdr (assoc :port params))) ;; to concat to the server
         (buffer-name (format "%s%s://%s%s/%s"
                              (if (string= session "none") ""
                                (format "[%s] " session))
                              engine
                              (if sql-user (concat sql-user "@") "")
                              (if sql-server (concat sql-server ":") "")
                              sql-database))
         (ob-sql-buffer (format "*SQL: %s*" buffer-name))
         )

    ;; predicate is set when sql-interactive-mode is on
    ;; todo: just check the buffer process status
    ;; so we don't have to turn sql-interactive on
    (if (sql-buffer-live-p ob-sql-buffer)
        (progn
          ;; set again the filter
          (set-process-filter (get-buffer-process ob-sql-buffer)
                              #'ob-sql-session-comint-output-filter)
          ;; and return the buffer
          ob-sql-buffer)

      ;; otherwise initiate a connection
      (save-window-excursion
        (setq ob-sql-buffer
              (ob-sql-connect engine buffer-name session-p) ; start the client
              ))

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

        ;; SQL interactive terminal starts.
        ;; When setting a process filter, the output gets redirected

        ;; set the redirection filter
        (set-process-filter sql-term-proc
                            #'ob-sql-session-comint-output-filter)
        ;; return the buffer
        (get-buffer ob-sql-buffer)
        ))))


(defun ob-sql-connect (&optional engine sql-cnx session-p)
  "Run ENGINE interpreter as an inferior process.

Imported from sql.el with a few modification in order
to prompt for authentication only if there's a missing
parameter. Depending on the sql client the password
should also be prompted. "

  ;; Get the value of engine that we need
  (setq sql-product
        (cond
         ((assoc engine sql-product-alist) ; Product specified
          engine)
         (t sql-product)))              ; Default to sql-engine

  (if (not engine)
      (user-error "No default SQL engine defined: set `sql-product'")

    (when (sql-get-product-feature sql-product :sqli-comint-func)
      ;; If no new name specified or new name in buffer name,
      ;; try to pop to an active SQL interactive for the same engine
      (let ((buf (sql-find-sqli-buffer sql-product sql-connection))
            ;;    ;; We have a new name or sql-buffer doesn't exist or match
            ;;    ;; Start by remembering where we start
            (prompt-regexp (sql-get-product-feature engine :prompt-regexp ))
            (prompt-cont-regexp (sql-get-product-feature engine :prompt-cont-regexp))
            (start-buffer (current-buffer))
            sqli-buffer rpt
            )

        ;; store the regexp used to clear output (prompt1|indicator|prompt2)
        (sql-set-product-feature
         engine :ob-sql-session-clear-output
         ( concat "\\(" prompt-regexp "\\)"
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
              ;; default value was nil ?
              (sql-prompt-regexp (or (default-value 'sql-prompt-regexp) ""))
              (default-directory
               (or sql-default-directory
                   default-directory)))

          ;; The password wallet returns a function which supplies the password.
          (when (functionp sql-password)
            (setq sql-password (funcall sql-password)))

          ;; Call the COMINT service
          (setq
           sqli-buffer
           (funcall (sql-get-product-feature engine :sqli-comint-func)
                    engine
                    (sql-get-product-feature engine :sqli-options)
                    (format "SQL: %s" sql-cnx)))
          ;; no need for a numbered buffer:
          ;; connexion is closed, buffer killed when there's no session
          ;; engine/user/db/session points to the same buffer otherwise

          ;; Set SQLi mode.
          (when sql-database ;; sql-for-each-login needs a db param or fails
            (let ((sql-interactive-product engine)) (sql-interactive-mode)))

          (setq-local sql-buffer (buffer-name sqli-buffer))

          ;; Set `sql-buffer' in the start buffer
          (with-current-buffer start-buffer
            (when (derived-mode-p 'sql-mode)
              (setq sql-buffer (buffer-name sqli-buffer))
              (run-hooks 'sql-set-sqli-hook)))


          ;; complete login
          ;; todo: replace with a comint filter
          ;; don't look for the prompt and give it a max time
          (let ((proc (get-buffer-process sqli-buffer))
                (secs sql-login-delay)
                (step 0.2))
            (while (and proc
                        (memq (process-status proc) '(open run))
                        (or (accept-process-output proc step)
                            (<= 0.0 (setq secs (- secs step))))
                        (progn (goto-char (point-max))
                               (not (re-search-backward sql-prompt-regexp 0 t))))
              (sql-progress-reporter-update rpt)))

          (run-hooks 'sql-login-hook))

        (sql-progress-reporter-done rpt)

        (get-buffer sqli-buffer)
        ))))



(defun ob-sql-send-string (str buffer)
  "Process then send the command STR to the SQL process."
  (let ((s (concat
            ;; join as a one line command
            (replace-regexp-in-string
             "[\s\t]*\n" " "
             ;; or the process will treat newlines as <enter>
             ;; no matter what, and then stop on error won't work
             ;; It works, but risky for input that contains newline
             ;; and harder to debug
             ;; the best option would be 1)
             (replace-regexp-in-string
              ;; tabs are interperted as command completion
              "[\t]+" " "
              (replace-regexp-in-string
               "\s*--.*\n" "" ;; remove comments
               (replace-regexp-in-string
                "^\s*$" "" ;; strip blank lines
                str))))
            "\n"
            (sql-get-product-feature sql-product :batch-terminate))
           ))
    (message ">>> %s" s)
    (process-send-string (get-buffer-process buffer) s)))


(defun ob-sql-session-comint-output-filter (proc string)
  "Process output gets redirected in a temporary buffer.It is called
several times consecutively as the shell outputs and flush its message
buffer"

  (with-local-quit

    (message string)
    ;; Inserting the result in the sql process buffer
    ;; adds it to the terminal prompt and as a result
    ;; the ouput gets passed as input onto the next command
    ;; line; See `comint-redirect-setup' to possibly fix that
    ;; (with-current-buffer (process-buffer proc) (insert output))

    (when (string-match ob-sql-session--batch-end-indicator string)
      (setq ob-sql-session-command-terminated t))

    (with-current-buffer (get-buffer-create "*ob-sql-result*")
      (insert string))))

(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("sql-session" . sql))
  (add-to-list 'org-babel-tangle-lang-exts '("sql-session" . "sql"))
  ;;(add-to-list 'org-structure-template-alist '("sql" . "src sql-mode")
  ) ;; or (customize-variable 'org-structure-template-alist)


(provide 'ob-sql-session)

;; LocalWords: sql org-mode session
;;; ob-sql-session.el ends here
