;;; Code:
(require 'sqlite)

(defvar bq-tables '("buffers"))
(defvar bq-actions '("select" "mark" "kill" "open" "save" "delete"))
(defvar bq-conditionals '("=" "!=" "and" "not" "in" "like"))
(defvar buffer-columns '("crm" "name" "size" "mode" "file"))
(defvar BQBuffers "")
(defvar BQQuery "")
(defvar BQSetup nil)
(defvar BQ-DBconnection "")
(defvar BQ-SQLfile "~/.emacs.d/buffer-query/bqbuffers.db")
(defvar BQ-Metadata-file "~/.emacs.d/buffer-query/bq-data.el")

(put 'BQQuery 'query-history '())

(defun bq--get-buffer-data ()
  "Inital Call to get Buffers metadata and store in symbol BQBuffers."
  (let (bq-start-buffer)
  (setq bq-start-buffer (current-buffer))
  (if (get-buffer "*Buffer List*")
      (progn
	(if (equal (current-buffer) (get-buffer "*Buffer List*"))
	    (progn
	      (switch-to-buffer (next-buffer))
	      (kill-buffer "*Buffer List*")
	      (buffer-menu)
	      (setq bq-start-buffer (current-buffer))
	      (switch-to-buffer bq-start-buffer))
	  (progn
	    (kill-buffer "*Buffer List*")
	   (buffer-menu)
	   (switch-to-buffer bq-start-buffer))))
      (progn (buffer-menu)
	     (switch-to-buffer bq-start-buffer)))))
 
(defun bq-query()
  "User exposed function for querying to buffers."
  (interactive)
  (setq BQQuery nil)
  (put 'BQQuery 'where nil)
  (bq--get-buffer-data)
  (let (input split-query xyz (iter 0) columns conditions)
    (setq input (read-string "command:" nil (get 'BQQuery 'query-history) nil))
    (setq split-query (split-string input " "))
    (put 'BQQuery 'query input)
    (push input (get 'BQQuery 'query-history))
    (dolist (xyz split-query)
      (cond
       ((member xyz bq-tables) (put 'BQQuery 'table xyz))
       ((string= xyz "where") (put 'BQQuery 'where xyz))
       ((member xyz bq-conditionals) (push xyz conditions))
       ((member xyz buffer-columns) (push xyz columns)) ;; TODO if you do select name where name = x, it will select name twice;; this one is only here because we only have one 'table'
       ))
    (put 'BQQuery 'condition conditions)
    (put 'BQQuery 'columns columns)
    (if (member (nth 0 split-query) bq-actions)
	(cond ((cl-search (nth 0 split-query) "select") (bq-select))
	      ((cl-search (nth 0 split-query) "mark") (bq-mark))
	      ((cl-search (nth 0 split-query) "kill") (bq-kill))
	      ((cl-search (nth 0 split-query) "delete") (bq-delete))
	      ((cl-search (nth 0 split-query) "save") (bq-save))
	      ((cl-search (nth 0 split-query) "open") (bq-open)))
      (message "sorry, not a valid statement..."))))


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

(defun bq-delete-buffer (buffer)
  "Delete BUFFER."
  (let (s-test (iter 0) buffer-list-line)
    (progn
      (switch-to-buffer "*Buffer List*")
      (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
      (dolist (buffer-list-line (split-string s-test "\n"))
	(if (and (cl-search (buffer-name buffer) buffer-list-line) (cl-search (number-to-string (buffer-size buffer)) buffer-list-line))
	    (progn (move-to-window-line iter) (Buffer-menu-delete)))
	(setq iter (+ iter 1))))))

(defun bq-save-buffer (buffer)
  "Save BUFFER."
  (let (s-test (iter 0) buffer-list-line)
    (progn
      (switch-to-buffer "*Buffer List*")
      (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
      (dolist (buffer-list-line (split-string s-test "\n"))
	(if (and (cl-search (buffer-name buffer) buffer-list-line) (cl-search (number-to-string (buffer-size buffer)) buffer-list-line))
	    (progn (move-to-window-line iter) (Buffer-menu-save)))
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

(defun bq-kill ()
  "kill."
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
       ((equal cond-set t) (kill-buffer buff))))))

(defun bq-open ()
  "Open."
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
       ((equal cond-set t)
	(progn
	  (switch-to-buffer buff)
	  (catch 'return
	    (when t
	      (throw 'return t)))))))))

(defun bq-delete ()
  "Delete."
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
       ((equal cond-set t) (bq-delete-buffer buff))))))

(defun bq-save ()
  "Save."
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
       ((equal cond-set t) (bq-save-buffer buff))))))

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
    (cond ;;"crm" "name" "size" "mode" "file"
     ((string= column "name") (setq column-value (string= match (buffer-name buffer))))
     ((string= column "size") (setq column-value (= match (buffer-size buffer))))
     ((string= column "mode") (setq column-value (string= match (symbol-name (buffer-local-value 'major-mode (get-buffer buffer))))))
     ((string= column "file")
      (if (cl-search "dired" (symbol-name (buffer-local-value 'major-mode (get-buffer buffer))))
	  (with-current-buffer buffer
	    (setq column-value (string= match default-directory)))
	(setq column-value (string= match (buffer-file-name buffer)))))
     (t (setq column-value nil)))
    (catch 'return
    (when t
      (throw 'return column-value)))))

(defun bq-!= (column match buffer)
  "Test if MATCH string and COLUMN value from BUFFER are not equal."
  (let (column-value)
    (cond ;;"crm" "name" "size" "mode" "file"
     ((string= column "name") (setq column-value (not (string= match (buffer-name buffer)))))
     ((string= column "size") (setq column-value (not (= match (buffer-size buffer)))))
     ((string= column "mode") (setq column-value (not (string= match (symbol-name (buffer-local-value 'major-mode (get-buffer buffer)))))))
     ((string= column "file")
      (if (cl-search "dired" (symbol-name (buffer-local-value 'major-mode (get-buffer buffer))))
	  (with-current-buffer buffer
	    (setq column-value (not (string= match default-directory))))
	(setq column-value (not (string= match (buffer-file-name buffer))))))
     (t (setq column-value nil)))
    (catch 'return
    (when t
      (throw 'return column-value)))))


(defun bq-in (column match buffer)
  "Test if MATCH string and COLUMN value from BUFFER are equal."
  (let (column-value)
    (cond ;;"crm" "name" "size" "mode" "file"
     ((string= column "name") (setq column-value (not (equal (cl-search match (buffer-name buffer)) nil))))
     ((string= column "size") (setq column-value (not (equal (cl-search match (number-to-string (buffer-size buffer))) nil))))
     ((string= column "mode") (setq column-value (not (equal (cl-search match (symbol-name (buffer-local-value 'major-mode (get-buffer buffer)))) nil))))
     ((string= column "file")
      (if (cl-search "dired" (symbol-name (buffer-local-value 'major-mode (get-buffer buffer))))
	  (with-current-buffer buffer
	    (setq column-value (not (equal (cl-search match default-directory) nil))))
	(setq column-value (not (equal (cl-search match (buffer-file-name buffer)) nil )))))
     (t (setq column-value nil)))
    (catch 'return
    (when t
      (throw 'return column-value)))))

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
