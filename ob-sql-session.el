;;; ob-sql-session.el --- Babel Functions for SQL, with session support -*- lexical-binding: t -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Phil Estival pe@7d.nz
;; URL: http://github.com/flintforge/ob-sql-session
;; Version: 1.2
;; Package-Requires: ((emacs "28.2")) ((org "9.6"))
;; Keywords: languages, org, org-babel, sql

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating SQL using sql source code with sessions.
;;
;; This replace the previous proposed communication channel set with
;; the comint sql interpreter using `accept-process-output' by a
;; comint output filter through `set-process-filter'.
;; The filter is set during the commands execution and removed on exit.
;; The interactive command interpreter remains usable.
;; 
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
;; Handling the case of Stored Procedures is beyond the scope of
;; ob-sql.

;; Not implemented
;; - dbconnection
;; - dbpassword : for postgres, will still be prompted
;;                on the first connection


;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ob)
(require 'org)
(require 'sql)


(defcustom org-babel-default-header-args:sql-session
  '((:engine . "ansi"))
  "Default header args."
  :group 'org-babel
  :safe t)

(defconst org-babel-header-args:sql-session
  '((engine     . :any)
    (dbhost     . :any)
    (dbport     . :any)
    (dbuser     . :any)
    (dbpassword . :any)
    (database   . :any)))

(defvar ob-sql-clean-output--regexp ""
  "clean prompts and extra characters in the shell")

(defun org-babel-execute:sql-session (body params)
  "Execute SQL statements in BODY using PARAMS."

  (let* (
         (processed-params (org-babel-process-params params))
         (engine (cdr (assoc :engine processed-params)))
         (session (cdr (assoc :session processed-params)))
         (vars (org-babel--get-vars params))
         (results (split-string (cdr (assq :results params ))))
         (sql--buffer (org-babel-sql-session-session
                       (intern engine) session processed-params))        
         )

    ;; Variables
    ;; Substitute $[:alnum:] in body by the associated
    ;; value.
    ;; We don't insert new variables. ANSI SQL has none,
    ;; Postgres has stateful variables.
    ;; Handling the case of Stored Procedures
    ;; is beyond the scope of ob-sql.

    (mapcar
     (lambda(v) (setq body (string-replace
                       (concat "$"(symbol-name(car v)))(cdr v) body)))
     vars)

    
    (with-current-buffer (get-buffer-create "*ob-sql-result*")
      (erase-buffer))

    (setq org-babel-sql-hold-on '(t))
    ;;(process-send-string sql--buffer body) ;; also works but needs processing
    (ob-sql-send-string body (get-buffer sql--buffer))
    (sleep-for 0.1)
    ;; let say that the first command will
    ;; take 1/10th s, while the rest of the output
    ;; will burst every 50ms 
    (while (pop org-babel-sql-hold-on) (sleep-for 0.05))
    ;; remove filter
    (set-process-filter (get-buffer-process sql--buffer) nil)

    ;; get results
    (with-current-buffer (get-buffer-create "*ob-sql-result*")
      ;; The output is clean by the filter
      ;; (replace-regexp ob-sql-clean-output--regexp "" nil nil nil t)
      ;; would replace backward as we're at the end of input. 
      
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


(defun org-babel-sql-session-session (engine session params)
  "Starts the SQL client if it has not in a buffer
named *SQL: [engine]:[user@server:/database]*
clear the intermediate buffer from previous output,
and set the process filter.
Return the comint process buffer."

  (let* ((sql-database (cdr (assoc :database params)))
         (sql-user (cdr (assoc :dbuser params)))
         (sql-password (cdr (assoc :dbpassword params)))
         (sql-server (cdr (assoc :dbserver params)))
				 (session-p (not (string= session "none")))
         (buffer-name (format "%s%s://%s%s/%s"
                              (if (string= session "none") ""
                                (format "[%s] " session))
                              engine
															(if sql-user (concat sql-user "@") "")
															(if sql-server (concat sql-server ":") "")
															sql-database))

         (ob-sql-buffer (format "*SQL: %s*" buffer-name))
         )

    ;; session exists ?
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
            (prompt-regexp (sql-get-product-feature engine :prompt-regexp))
						(prompt-cont-regexp (sql-get-product-feature engine :prompt-cont-regexp)))
        
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

					;; not global, should be in a product feature or in an alist of the session
          ;; Tells what the cleaning regexp is (the prompts)
          (setq ob-sql-clean-output--regexp
                ( concat prompt-regexp
                  (when prompt-cont-regexp "\\|" prompt-cont-regexp)))
          ;;( concat (prompt-regexp "") "\\|" prompt-cont-regexp))
          
          ;; clear the welcoming message out of the output from the
          ;; first command, in case we forgot the quiet mode.
          ;; we can't evaluate how long the connection will take
          ;; so if quiet mode is off and the connexion takes time
          ;; then the welcoming message may show up
          (sleep-for 0.2)
          (with-current-buffer (get-buffer ob-sql-buffer) (erase-buffer))

          ;; SQL interactive terminal starts.
          ;; When setting a process filter, the output gets redirected     
          
          ;; set the redirection filter
          (set-process-filter sql-term-proc
                              #'ob-sql-session-comint-output-filter)          
          ;; return the buffer name
          ob-sql-buffer
          )))))


(defun ob-sql-connect (&optional product output-buffer session-p)
  "Run PRODUCT interpreter as an inferior process.

Imported from sql.el with a few modification in order
to prompt for authentication only if there's a missing
parameter. Depending on the sql client the password
may also be prompted.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer `*SQL*'."

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
        (let ((buf (sql-find-sqli-buffer sql-product sql-connection)))
          ;;    ;; We have a new name or sql-buffer doesn't exist or match
          ;;    ;; Start by remembering where we start
          (let ((start-buffer (current-buffer))
                new-sqli-buffer rpt)

            ;; Get credentials.
            ;; either all fields are provided
						;; or there's a specific case were no login is needed
						;; or trigger the prompt
            (or (and sql-database sql-user sql-server ) ;sql-port?
								(eq sql-product 'sqlite) ;; sqlite allows in-memory db 
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
									;; it's default value is nil ?
                  (sql-prompt-regexp (or (default-value 'sql-prompt-regexp) ""))
                  (default-directory
                   (or sql-default-directory
                       default-directory)))
							
              ;; The password wallet returns a function which supplies the password.
              (when (functionp sql-password)
                (setq sql-password (funcall sql-password)))
							
							(message "%s %s" output-buffer session-p)
              ;; Call the COMINT service
							(setq new-sqli-buffer 
							(funcall (sql-get-product-feature product :sqli-comint-func)
                       product
                       (sql-get-product-feature product :sqli-options)
                       ;; generate a buffer name

                       (cond
												(session-p (format "*SQL: %s" buffer-name))
                        ((not output-buffer)
                         (sql-generate-unique-sqli-buffer-name product nil))
                        ((consp output-buffer)
                         (sql-generate-unique-sqli-buffer-name
													product (read-string
																	 "Buffer name (\"*SQL: XXX*\"; enter `XXX'): "
																	 (sql-make-alternate-buffer-name product))))
                        ((stringp output-buffer)
                         (if (or (string-prefix-p " " output-buffer)
                                 (string-match-p "\\`[*].*[*]\\'" output-buffer))
                             output-buffer
                           (sql-generate-unique-sqli-buffer-name product output-buffer)))
                        (t
                         (sql-generate-unique-sqli-buffer-name product output-buffer)))))

							;; Set SQLi mode.
							;; why would we need this ?
							;;(let ((sql-interactive-product product)) (sql-interactive-mode))
							
							;; Set the new buffer name
							
							(setq-local sql-buffer (buffer-name new-sqli-buffer))

							;; Set `sql-buffer' in the start buffer
							(with-current-buffer start-buffer
								(when (derived-mode-p 'sql-mode)
									(setq sql-buffer (buffer-name new-sqli-buffer)) 
									(run-hooks 'sql-set-sqli-hook)))

							;; Make sure the connection is complete
							;; (Sometimes start up can be slow)
							;;  and call the login hook
							(let ((proc (get-buffer-process new-sqli-buffer))
										(secs sql-login-delay)
										(step 0.3))
								(while (and proc
														(memq (process-status proc) '(open run))
														(or (accept-process-output proc step)
																(<= 0.0 (setq secs (- secs step))))
														;;sql-prompt-regexp ;; nil prompt ?
														(progn (goto-char (point-max))
																	 (not (re-search-backward sql-prompt-regexp 0 t))))
									(sql-progress-reporter-update rpt)))

							(goto-char (point-max)) ;; 
							(when (re-search-backward sql-prompt-regexp nil t)
								(run-hooks 'sql-login-hook))
							
							(sql-progress-reporter-done rpt)
							(goto-char (point-max))

							(let ((sql-display-sqli-buffer-function t))
								(sql-display-buffer new-sqli-buffer))
							(get-buffer new-sqli-buffer)
							))))
		(user-error "No default SQL product defined: set `sql-product'")))


(defun ob-sql-session-comint-output-filter (proc string)
	"Process output gets redirected in a temporary buffer.It is called
several times consecutively as the shell outputs and flush its message
buffer"

  (push 0 org-babel-sql-hold-on)
  (with-local-quit
    (let ((output (replace-regexp-in-string
                   ob-sql-clean-output--regexp ""
                   string nil 'literal)))
      
      ;; inserting the result in the sql process buffer,
      ;; renders the terminal stdin unusable as those
      ;; ouput gets passed as input on the next command
      ;; line (and I don't know how to handle the equivalent
			;; of stderr in emacs...)
       (with-current-buffer (process-buffer proc) (insert output))
      
      (with-current-buffer (get-buffer-create "*ob-sql-result*")
        (insert output)))))


(defun ob-sql-send-string (str buffer)
  "Send the string STR to the SQL process.
Simplified version of `sql-send-string'"
  (let ((s (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" str)))
      (with-current-buffer buffer
        (insert "\n")
        (comint-set-process-mark)
        ;; Send the string, trim trailing whitespace
        (sql-input-sender (get-buffer-process (current-buffer)) s)
        ;; Send a command terminator 
        (sql-send-magic-terminator buffer s sql-send-terminator))))


(add-to-list 'org-babel-tangle-lang-exts '("sql-session" . "sql"))

(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("sql-session" . sql)))

(provide 'ob-sql-session)

;; LocalWords: SQL Org-mode session
;;; ob-sql-session.el ends here

