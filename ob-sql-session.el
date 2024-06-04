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
;; Stored procedures are beyond the scope of ob-sql.

;; Not implemented
;; - dbconnection
;; - dbpassword : on postgres, will still be prompted
;;                on the first connection

;; Since this is labeled with "reproducible research" and "litterate
;; programming", one may consider that a "session" implies an on-going
;; state, that, in order to be reproducible, must preserve a sequence.

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


;; the command that terminate a batch of commands
;; here it's \echo -----;
;; It's possible to hold the command while output goes on.  But
;; since the prompt is showing off on every commands there's no
;; way to figure out when batch of commands has terminated unless
;; a special command is inserted in its end; However, if a stop
;; on error is set, then the only remaining case is to also look
;; for (depending on the client) an ^ERROR: string indicating the
;; command has terminated The latter isn't implemented.
;; Not very elegant, and problematic when headers are on.
;; but I'm out of ideas. looking at ob-pyton-async perhaps ?

(defvar ob-sql-session--batch-end-indicator  "```"  "indicate the end of a command batch")

(sql-set-product-feature 'postgres :batch-terminate
												 (format "\\echo %s\n" ob-sql-session--batch-end-indicator))

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
														;; "-A"
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
      ;; output is cylean by the filter
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

  (let* ((sql-database	(cdr (assoc :database params)))
         (sql-user			(cdr (assoc :dbuser params)))
         (sql-password	(cdr (assoc :dbpassword params)))
         (sql-server		(cdr (assoc :dbhost params)))
         (buffer-name (format "%s%s://%s%s/%s"
                              (if (string= session "none") ""
                                (format "[%s] " session))
                              engine
															(if sql-user (concat sql-user "@") "")
															(if sql-server (concat sql-server ":") "")
															sql-database))
         (ob-sql-buffer (format "*SQL: %s*" buffer-name))
         )

		(message "B %s ?%s??" ob-sql-buffer (sql-buffer-live-p ob-sql-buffer))
		(sql-buffer-live-p (get-buffer "*SQL: [PG] postgres:///test*"))
		
		;; predicate is set when sql-interactive-mode is on
		;; todo: just check the buffer process status
    (if (sql-buffer-live-p ob-sql-buffer)
        (progn
          ;; set again the filter
          (set-process-filter (get-buffer-process ob-sql-buffer)
                              #'ob-sql-session-comint-output-filter)
          ;; and return the buffer
          ob-sql-buffer)

      ;; otherwise initiate the connection
      (let*(
            ;; (sql-port (cdr (assoc :port params))) ;; to concat to the server
						)

        (save-window-excursion
					(setq ob-sql-buffer
								(ob-sql-connect engine buffer-name session-p) ; start the client
								))
        ;;(call-interactively '(lambda()(sql-product-interactive "postgres" "sql")))
				
        (let ((sql-term-proc (get-buffer-process ob-sql-buffer))
              ;;(session-key (format "%s/%s/%s/%" engine session-buffer database user))
              )
          (unless sql-term-proc
            (user-error (format "SQL %s didn't start" engine)))

          ;; clear the welcoming message out of the output from the
          ;; first command, in case we forgot the quiet mode.
          ;; we can't evaluate how long the connection will take
          ;; so if quiet mode is off and the connexion takes time
          ;; then the welcoming message may show up
          (sleep-for 0.06)
					(with-current-buffer (get-buffer ob-sql-buffer) (erase-buffer))

          ;; SQL interactive terminal starts.
          ;; When setting a process filter, the output gets redirected

          ;; set the redirection filter
          (set-process-filter sql-term-proc
                              #'ob-sql-session-comint-output-filter)
          ;; return the buffer
          (get-buffer ob-sql-buffer)
          )))))


(defun ob-sql-connect (&optional product sql-cnx session-p)
  "Run PRODUCT interpreter as an inferior process.

Imported from sql.el with a few modification in order
to prompt for authentication only if there's a missing
parameter. Depending on the sql client the password
should also be prompted. "

  ;; Get the value of product that we need
  (setq sql-product
        (cond
         ((assoc product sql-product-alist) ; Product specified
          product)
         (t sql-product)))              ; Default to sql-product

  (if product
      (when (sql-get-product-feature sql-product :sqli-comint-func)
        ;; If no new name specified or new name in buffer name,
        ;; try to pop to an active SQL interactive for the same product
        (let ((buf (sql-find-sqli-buffer sql-product sql-connection))
							;;    ;; We have a new name or sql-buffer doesn't exist or match
							;;    ;; Start by remembering where we start
							(start-buffer (current-buffer))
							(sqli-buffer rpt)
							prompt-regexp (sql-get-product-feature 'postgres :prompt-regexp )
							prompt-cont-regexp (sql-get-product-feature 'postgres :prompt-cont-regexp )
							)

					;; store the regexp used to clear output
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
										 (sql-get-product-feature product :sqli-login)))
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
						 (funcall (sql-get-product-feature product :sqli-comint-func)
											product
											(sql-get-product-feature product :sqli-options)
											(format "SQL: %s" sql-cnx)))
						;; no need for a numbered buffer:
						;; connexion is closed, buffer killed when there's no session
						;; engine/user/db/session points to the same buffer otherwise

						;; Set SQLi mode.
						(let ((sql-interactive-product product)) (sql-interactive-mode))
						
						(setq-local sql-buffer (buffer-name sqli-buffer))

						;; Set `sql-buffer' in the start buffer
						(with-current-buffer start-buffer
							(when (derived-mode-p 'sql-mode)
								(setq sql-buffer (buffer-name sqli-buffer))
								(run-hooks 'sql-set-sqli-hook)))
						
						;; Make sure the connection is complete
						;; (Sometimes start up can be slow)
						;;  and call the login hook
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
					;; (goto-char (point-max))

					(get-buffer sqli-buffer)
					)))
	(user-error "No default SQL product defined: set `sql-product'"))


(defun ob-sql-send-string (str buffer)
  "Process then Send the command STR to the SQL process.
There is more to do here"
	(let ((s (concat
						(replace-regexp-in-string
						 ;; or the process will treat newlines as <enter>
						 ;; no matter what, and then stop on error won't work
						 ";\\([\s\t]*\n\\)*" "; "
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
		(message "---< %s" s)

		;;(let ((s (org-babel-chomp str)))
    (with-current-buffer buffer
      ;;(insert "\n")
      ;;(comint-set-process-mark)
      ;; Send the string, trim trailing whitespace
																				;(comint-accumulate);; (get-buffer-process (current-buffer)) s)
			;;(comint-send-input (get-buffer-process (current-buffer)) s)

			;; display it in the input buffer
			;;(insert s)
			;;(comint-send-input)

			;; no way to stop on error...
      ;;(sql-input-sender (get-buffer-process (current-buffer)) s)
      (process-send-string (get-buffer-process (current-buffer)) s)
      ;; Send a command terminator
      ;;(sql-send-magic-terminator buffer s sql-send-terminator)
			)))


(defun ob-sql-session-comint-output-filter (proc string)
	"Process output gets redirected in a temporary buffer.It is called
several times consecutively as the shell outputs and flush its message
buffer"

  (with-local-quit
			
			(m "%s" string)
			;; Inserting the result in the sql process buffer
      ;; add it to the terminal prompt and
      ;; the ouput gets passed as input on the next command
      ;; line; See `comint-redirect-setup' to possibly fix that
      ;;(with-current-buffer (process-buffer proc) (insert output))

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
