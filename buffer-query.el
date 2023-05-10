;;; Code:
(require 'sqlite)

(defvar bq-tables '("buffers"))
(defvar bq-actions '("select" "mark" "kill" "open" "copy" "save" "delete"))
(defvar bq-conditionals '("=" "!=" "and" "not" "in" "like"))
(defvar buffer-columns '("crm" "name" "size" "mode" "file"))
(defvar BQBuffers "")
(defvar BQQuery "")
(defvar BQSetup nil)
(defvar BQ-DBconnection "")
(defvar BQ-SQLfile "~/.emacs.d/buffer-query/bqbuffers.db")
(defvar BQ-Metadata-file "~/.emacs.d/buffer-query/bq-data.el")

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
	      (message "na")))))))
 


(defun bq-query()
  "User exposed function for querying to buffers."
  (interactive)
  (setq BQQuery nil)
  (put 'BQQuery 'where nil)
  (bq--get-buffer-data)
  ;; (bq--save) ;; TODO do we need to save and load?
  (let (input split-query xyz (iter 0) columns conditions)
    (setq input (read-string "command:" nil nil nil))
    (setq split-query (split-string input " "))
    (put 'BQQuery 'query input)
    ;; (push input (get 'BQQuery 'query-history input)) ;; TODO if 'query-history is nil, declare it with an empty list
    (dolist (xyz split-query)
      ;; (setq iter (+ iter 1))
      (cond
       ((member xyz bq-tables) (put 'BQQuery 'table xyz))
       ((string= xyz "where") (put 'BQQuery 'where xyz))
       ((member xyz bq-conditionals) (push xyz conditions))
       ((member xyz buffer-columns) (push xyz columns)) ;; this one is only here because we only have one 'table'
       ))
    (put 'BQQuery 'condition conditions)
    (put 'BQQuery 'columns columns)
    (if (member (nth 0 split-query) bq-actions)
	(cond ((cl-search (nth 0 split-query) "select") (bq-select))
	      ((cl-search (nth 0 split-query) "mark") (bq-mark))
	      ((cl-search (nth 0 split-query) "kill") (bq-kill))
	      ((cl-search (nth 0 split-query) "save") (bq-save))
	      ((cl-search (nth 0 split-query) "open") (bq-open)))
      (message "sorry, not a valid statement..."))))

(defun check-marked-buffer (buffer) "Check if BUFFER marked." (string "yes"))


(defun bq-mark-buffer (buffer)
  "Mark BUFFER."
  (let (s-test (iter 0) buffer-list-line)
    (progn
      (switch-to-buffer "*Buffer List*")
      (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
      (dolist (buffer-list-line (split-string s-test "\n"))
	(if (and (cl-search (buffer-name buffer) buffer-list-line) (cl-search (number-to-string (buffer-size buffer)) buffer-list-line))
	    (progn (move-to-window-line iter) (Buffer-menu-mark)))
	(setq iter (+ iter 1))))))

(defun bq-mark ()
  "Marking."
  (message "marking")
  (setq BQStatements '())
  (let (cond-set ww buff (buffer-res '()) conds current-statement stmt)
    (if (get 'BQQuery 'where) ;; if where conditional
	(progn
	  (setq cond-set nil)
	  (dolist (ww (split-string (nth 1 (split-string (get 'BQQuery 'query) "where ")) " "))
	    (message ww)
	    (cond
	     ((member ww bq-conditionals) (put 'current-statement 'cond ww))
	     ((member ww buffer-columns) (put 'current-statement 'col ww))
	     (t (put 'current-statement 'match ww)))
	    (push 'current-statement BQStatements)))
      (setq cond-set t))
    (dolist (buff (buffer-list))
      (setq conds '())
      (setq buffer-res '())
      (dolist (stmt BQStatements)
	(push (funcall (intern (concat "bq-" (get stmt 'cond))) (get stmt 'col) (get stmt 'match) buff) conds))
      (if (member nil conds)
	  (setq cond-set nil)
	(setq cond-set t))
      (cond
       ((equal cond-set t) (bq-mark-buffer buff))))))

(defun bq-select ()
  "Select function for bq-query."
  (setq BQStatements '())
  (let (selectors buff sel (res '()) (buffer-res '()) (conds '()) cond-set ww current-statement stmt)
    (cond ;; find columns we are selecting
     ((get 'BQQuery 'columns) (setq selectors (get 'BQQuery 'columns)))
     ((cl-search "*" (get 'BQQuery 'query)) (setq selectors buffer-columns)))
    (if (get 'BQQuery 'where) ;; if where conditional
	(progn
	  (setq cond-set nil)
	  (dolist (ww (split-string (nth 1 (split-string (get 'BQQuery 'query) "where ")) " "))
	    (cond
	     ((member ww bq-conditionals) (put 'current-statement 'cond ww))
	     ((member ww buffer-columns) (put 'current-statement 'col ww))
	     (t (put 'current-statement 'match ww)))
	    (push 'current-statement BQStatements)))
      (setq cond-set t))
    (dolist (buff (buffer-list))
      (setq conds '())
      (setq buffer-res '())
      (dolist (stmt BQStatements)
	(push (funcall (intern (concat "bq-" (get stmt 'cond))) (get stmt 'col) (get stmt 'match) buff) conds))
      (if (member nil conds)
	  (setq cond-set nil)
	(setq cond-set t))
      (dolist (sel selectors)
	(cond
	 ((and (string= "crm" sel) cond-set)  (push "yes" buffer-res))
	 ((and (string= "name" sel) cond-set) (push (buffer-name buff) buffer-res))
	 ((and (string= "size" sel) cond-set) (push (buffer-size buff) buffer-res))
	 ((and (string= "mode" sel) cond-set) (push (buffer-local-value 'major-mode (get-buffer buff)) buffer-res))
	 ((and (string= "file" sel) cond-set) (if (not (equal (buffer-file-name buff) nil)) (push (buffer-file-name buff) buffer-res)))
	 (t "na")))
      (if (length> buffer-res 0) (push buffer-res res)))
    (message "res: %s" res)))

(defun bq-= (column match buffer)
  "Test if MATCH string and COLUMN value from BUFFER are equal."
  (let (column-value)
    ;; I need to put this function in the buffer loop
    (cond ;;"crm" "name" "size" "mode" "file"
     ((string= column "name") (setq column-value (string= match (buffer-name buffer))))
     ((string= column "size") (setq column-value (= match (buffer-size buffer))))
     ((string= column "mode") (setq column-value (buffer-local-value 'major-mode (get-buffer buff))))
     ((string= column "size") (setq column-value (string= (buffer-file-name buffer))))
     (t (setq column-value nil)))
    (catch 'return
    (when t
      (throw 'return column-value)))))

(defun bq-!= (column match buffer)
  "Test if MATCH string and COLUMN value from BUFFER are not equal."
  (let (column-value)
    ;; I need to put this function in the buffer loop
    (cond ;;"crm" "name" "size" "mode" "file"
     ((string= column "name") (setq column-value (not (string= match (buffer-name buffer)))))
     ((string= column "size") (setq column-value (not (= match (buffer-size buffer)))))
     ((string= column "mode") (setq column-value (not (buffer-local-value 'major-mode (get-buffer buff)))))
     ((string= column "size") (setq column-value (not (string= (buffer-file-name buffer)))))
     (t (setq column-value nil)))
    (catch 'return
    (when t
      (throw 'return column-value)))))


(defun bq-in (column match buffer)
  "Test if MATCH string and COLUMN value from BUFFER are equal."
  (let (column-value)
    ;; I need to put this function in the buffer loop
    (cond ;;"crm" "name" "size" "mode" "file"
     ((string= column "name") (setq column-value (not (equal (cl-search match (buffer-name buffer)) nil))))
     ((string= column "size") (setq column-value (not (equal (cl-search match (number-to-string (buffer-size buffer))) nil))))
     ((string= column "mode") (setq column-value (buffer-local-value 'major-mode (get-buffer buff)))) ;; TODO fix
     ((string= column "size") (setq column-value (not (equal (cl-search match (buffer-file-name buffer)) nil))))
     (t (setq column-value nil)))
    (catch 'return
    (when t
      (throw 'return column-value)))))

;; (let (ztest
;;       col
;;       (buffer-cols '("crm" "name" "size" "mode" "file"))
;;       qselected
;;       (res '()))
;;   (switch-to-buffer "*Buffer List*")
;;   (setq qselected "name")
;;   (setq ztest (buffer-substring-no-properties (point-min) (point-max)))
;;   (dolist (bll (split-string ztest "\n"))
;;     (setq col (split-string bll "   "))
;;     ;; (message col)
;;     (message "%s" bll)
;;     (if (member qselected buffer-cols)
;; 	(push (nth (cl-position qselected buffer-cols :test 'equal) col) res))
;;   ;; (message (nth 100 res)) ;; if not found returns nil, which is nice
;;     )
;;   ;; (message "%s" res)
;;   )


;; (let (bbl s-test)
;;   (dolist (bbl (buffer-list))
;;     (if (cl-search "Open" (buffer-name bbl))
;; 	(progn
;; 	  (switch-to-buffer "*Buffer List*")
;; 	  (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
;; 	  (message (split-string s-test "\n"))
;; 	  (move-to-window-line 2)
;; 	  ;; (Buffer-menu-mark)
;; 	  )
;;       )))

 ;; (let (s-test (iter 0) buffer-list-line (buffer (get-buffer "project_ltg.php")))
 ;;    (progn
 ;;      (switch-to-buffer "*Buffer List*")
 ;;      (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
 ;;      (dolist (buffer-list-line (split-string s-test "\n"))
 ;; 	(if (and (cl-search (buffer-name buffer) buffer-list-line) (cl-search (number-to-string (buffer-size buffer)) buffer-list-line))
 ;; 	    (progn (move-to-window-line iter) (Buffer-menu-mark)))
 ;; 	(setq iter (+ iter 1)))))

;; (bq-in "name" ".php" (get-buffer "project_ltg.php"))



;;test cases
"select * from buffers where .py in name"
"select * from buffers where name = *scratch*"
"select * from buffers where name != *scratch*"
"mark * from buffers"
"kill * from buffers where name is not *scratch*"
"kill * from buffers where name is not like term"
"kill * from buffers where name like Open-projects"
"save buffers where name is main.py"
"open buffer where name main.py" ;; open will open first result



(provide 'buffer-query)
;;; buffer-query.el ends here
