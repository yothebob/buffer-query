;;; Code:
(defvar bq-tables '("buffers"))
(defvar bq-actions '("select" "mark" "kill" "open" "copy" "save" "delete"))
(defvar bq-conditionals '("where" "if" "and" "not" "like"))
(defvar bq-orders '("desc" "asc"))
(defvar buffer-columns '("crm" "name" "size" "mode" "file"))
(defvar BQBuffers)

(defun bq--get-buffer-data ()
  "Inital Call to get Buffers metadata and store in symbol BQBuffers."
  ;; get lines of buffers that are marked in the buffer list
  (let (bz buffers-marked bq-buffer bl-split-string (iter 0))
    (with-current-buffer "*Buffer List*"
      (setq bl-split-string (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
      (dolist (bz bl-split-string)
	(setq iter (+ iter 1))
	(if (> (length bz) 0)
	    (if (cl-search (substring bz 0 1) ">")
		(put 'bq-buffer 'marks iter) ;; saving the line in the buffer list.. maybe not great?
	      (message "na bra"))))))
  
  ;; get buffer names and save them
  (let (bz buffer-names bq-buffer)
    (dolist (bz (buffer-list))
      (push (buffer-name bz) buffer-names))
    (put 'bq-buffer 'names buffer-names)
    (message "%s" (get 'bq-buffer 'names)))
  
  ;; get buffer filenames and save them
  (let (bz buffer-filenames bq-buffer)
    (dolist (bz (buffer-list))
      (push (buffer-file-name bz) buffer-filenames))
    (put 'bq-buffer 'filenames buffer-filenames)
    (message "%s" (get 'bq-buffer 'filenames)))

  ;; get buffer sizes and save them
  (let (bz buffer-sizes bq-buffer)
    (dolist (bz (buffer-list))
      (push (buffer-size bz) buffer-sizes))
    (put 'bq-buffer 'sizes buffer-sizes)
    (message "%s" (get 'bq-buffer 'sizes)))
  )


(defun bq-query()
  (interactive)
  (let (query input)
    (setq input (read-string "command:" nil nil nil))
    (put 'query 'split-query (split-string input))
    (if (member (nth 0 (get 'query 'split-query)) bq-actions)
	(cond
	 ((cl-search (nth 0 (get 'query 'split-query)) "select") (bq-select (get 'query 'split-query)));; I want to use symbols here to call functions.. look into later
	 )
      (message "sorry, not a valid statement..."))
    )
  )

(defun bq-select (q-list)
"Allways assuming buffers table right now."
(let (q-column res q-conditionals)
  (cond
   ((member (nth 1 q-list) buffer-columns) (setq q-column '((nth 1 q-list))))
   ((string= (nth 1 q-list) "*") (setq q-column buffer-columns))
   )
   (dolist (bqr ))
  ))


(buffer-list)

(defun bb-query-buffers (query)
  "A user function where you can manage buffers with a sql like language."
  (interactive)
  (let (q-list
	q-word
	;; q-hash
	(iter 0)
	(action-words '("select" "mark" "kill" "open" "copy" "save" "delete")) ;; select will be querying
	(conditional-words '("where" "if" "and" "not" "like"))
	(order-words '("desc" "asc"))
	(to-order '("order"))
	(tables '("buffers"))
	(buffer-columns '("crm" "name" "size" "mode" "file")))
      (setq q-list (split-string query))
      (dolist (q-word q-list)
	(message q-word)
	
	(cond((member q-word action-words) (message "")))
	(setq iter (+ iter 1)))))
(bb-query-buffers "select * from buffers")

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





;; (put 'bzb-testz 'action "select")
;; (get 'bzb-testz 'action)

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
