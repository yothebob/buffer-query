;;; Code:
(require 'sqlite)

(defvar bq-tables '("buffers"))
(defvar bq-actions '("select" "mark" "kill" "open" "copy" "save" "delete"))
(defvar bq-conditionals '("where" "if" "and" "not" "like"))
(defvar bq-orders '("desc" "asc"))
(defvar buffer-columns '("crm" "name" "size" "mode" "file"))
(defvar BQBuffers "")
(defvar BQQuery "")
(defvar BQSetup nil)
(defvar BQ-DBconnection "")
(defvar BQ-SQLfile "~/.emacs.d/buffer-query/bqbuffers.db")
(defvar BQ-Metadata-file "~/.emacs.d/buffer-query/bq-data.el")

;; .... i mean i was thinking about writing my own sql parsing tool...
;; but hell.. I mean i could just use a sql lite file and actually use sql?
;; I think if I do both (as a pickable option) that would be cool. Because there is alot of churn in buffers.
;; I think maybe some people would just want it to run without the sqlite dependancies

;; FIRST TIME SETUPS
(defun bq--initial-setup()
  "Initial setup, seeing if you need sqlite3 installed, if you want to use sqlite or nosql."
  (let (u-input (choices '("y", "n")))
    (setq u-input (completing-read "Do you want to use SQL? (y/n):" choices))
    (cond
     ((string= u-input "y") (bqsql--initial-setup-sql))
     ((string= u-input "n") (message "continue as normal..."))
     ))
  (setq BQSetup t))

(defun bqsql--initial-setup-sql ()
  "Intial setup of sql."
  (let (sql-version)
    (setq sql-version (shell-command-to-string "sqlite --version"))
    (if (cl-search "not found" sql-version)
	(message "Please install sqlite3")
      (progn
	(message "else")
	(shell-command-to-string (format "touch %s" BQ-SQLfile))
	(setq BQ-DBconnection (sqlite-init BQ-SQLfile))))))



;; SAVE AND LOAD
(defun bq--save()
  "Save BQBuffers data to bq-data file."
  (let ((save-file-contents "(setq BQBuffers \"\")\n") sfc)
    (dolist (sfc (symbol-plist 'BQBuffers))
      (if (and (member sfc '(names filenames sizes marks)) (not (string= "" (symbol-name sfc))))
	  (setq save-file-contents (concat save-file-contents (format "(put 'BQBuffers '%s '%s)\n" (symbol-name sfc) (get 'BQBuffers sfc))))))
    (setq save-file-contents (concat save-file-contents (format "(provide 'bq-data)\n;;; bq-data.el ends here" (symbol-name sfc))))
    (with-temp-buffer
      (insert save-file-contents)
      (write-region (point-min) (point-max) BQ-Metadata-file))))

(defun bq--load()
  "Load metadata file and update symbols."
  (if (file-exists-p BQ-Metadata-file)(load BQ-Metadata-file)))

(defun bqsql--save()
  "Test.")

(defun bqsql--load()
  "Test.")



(defun bq--get-buffer-data ()
  "Inital Call to get Buffers metadata and store in symbol BQBuffers."
  (let (bq-start-buffer)
  (setq bq-start-buffer (current-buffer))
  (if (not (get-buffer "*Buffer List*"))
      (progn (buffer-menu)
	     (switch-to-buffer bq-start-buffer))))
  ;; get lines of buffers that are marked in the buffer list
  (let (bz (iter 0))
    (with-current-buffer "*Buffer List*"
      (dolist (bz (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
	(setq iter (+ iter 1))
	(if (> (length bz) 0)
	    (if (cl-search (substring bz 0 1) ">")
		(put 'BQBuffers 'marks iter)
	      (message "na"))))))
  ;; get buffer names and save them
  (let (bz buffer-names)
    (dolist (bz (buffer-list))
      (push (buffer-name bz) buffer-names))
    (put 'BQBuffers 'names buffer-names))
  ;; get buffer filenames and save them
  (let (bz buffer-filenames)
    (dolist (bz (buffer-list))
      (push (buffer-file-name bz) buffer-filenames))
    (put 'BQBuffers 'filenames buffer-filenames))
  ;; get buffer sizes and save them
  (let (bz buffer-sizes)
    (dolist (bz (buffer-list))
      (push (buffer-size bz) buffer-sizes))
    (put 'BQBuffers 'sizes buffer-sizes)))


(defun bq-query()
  (interactive)
  (bq--get-buffer-data)
  ;; (bq--save) ;; TODO do we need to save and load?
  (let (input split-query xyz iter columns)
    (setq input (read-string "command:" nil nil nil))
    (setq split-query (split-string input))
    (put 'BQQuery 'query input)
    ;; (push input (get 'BQQuery 'query-history input)) ;; TODO if 'query-history is nil, declare it with an empty list
    (dolist (yxz split-query)
      (setq iter (+ iter 1))
      (cond
       ((member xyz bq-tables) (put 'BQQuery 'table xyz))
       ((member xyz bq-orders) (put 'BQQuery 'order xyz))
       ((member xyz bq-conditionals) (put 'BQQuery 'condition xyz))
       ((member xyz bq-columns) (push xyz columns))))
    (put 'BQQuery 'columns columns)
    (if (member (nth 0 split-query) bq-actions)
	(cond ((cl-search (nth 0 split-query) "select") (bq-select))
	      ((cl-search (nth 0 split-query) "mark") (bq-mark))
	      ((cl-search (nth 0 split-query) "kill") (bq-kill))
	      ((cl-search (nth 0 split-query) "save") (bq-save))
	      ((cl-search (nth 0 split-query) "open") (bq-open)))
      (message "sorry, not a valid statement..."))))

(defun bq-mark ()
"Allways assuming buffers table right now Q-LIST."
(message "%s" (string q-list)))

(defun bq-select ()
"Allways assuming buffers table right now Q-LIST."
(message "%s" (string "sdfsdfds")))
;; (let (q-column res q-conditionals)
;;   (cond
;;    ((member (nth 1 q-list) buffer-columns) (setq q-column '((nth 1 q-list))))
;;    ((string= (nth 1 q-list) "*") (setq q-column buffer-columns))
;;    )
;;    (dolist (bqr ))
;;   ))

(defun bb-do-action (action query-list)
  "based off an action do "
  (message action query-list)
  )


(let (ztest
      col
      (buffer-cols '("crm" "name" "size" "mode" "file"))
      qselected
      (res '()))
  (switch-to-buffer "*Buffer List*")
  (setq qselected "name")
  (setq ztest (buffer-substring-no-properties (point-min) (point-max)))
  (dolist (bll (split-string ztest "\n"))
    (setq col (split-string bll "   "))
    ;; (message col)
    (message "%s" bll)
    (if (member qselected buffer-cols)
	(push (nth (cl-position qselected buffer-cols :test 'equal) col) res))
  ;; (message (nth 100 res)) ;; if not found returns nil, which is nice
    )
  ;; (message "%s" res)
  )



"select * from buffers where .py in name" ;;will show message of buffer data
""
"mark * from buffers"
"kill * from buffers where name is not *scratch*"
"kill * from buffers where name is not like term"
"kill * from buffers where name like Open-projects"
"save buffers where name is main.py"
"open buffer where name main.py" ;; open will open first result


(let (bbl s-test)
  (dolist (bbl (buffer-list))
    (if (cl-search "Open" (buffer-name bbl))
	(progn
	  (switch-to-buffer "*Buffer List*")
	  (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
	  (message (split-string s-test "\n"))
	  ;; (move-to-window-line 2)
	  ;; (Buffer-menu-mark)
	  )
      )))

(provide 'buffer-list-sql)
;;; buffer-list-sql.el ends here
