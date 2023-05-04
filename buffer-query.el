;;; Code:
(defvar bq-tables '("buffers"))
(defvar bq-actions '("select" "mark" "kill" "open" "copy" "save" "delete"))
(defvar bq-conditionals '("where" "if" "and" "not" "like"))
(defvar bq-orders '("desc" "asc"))


(defun bq-query()
  (interactive)
  (let (query input)
    (setq input (read-string "command:" nil nil nil))
    (put 'query 'split-query (split-string input))
    (if (member (nth 0 (get 'query 'split-query)) bq-actions)
	(cond
	 ((cl-search (nth 0 (get 'query 'split-query)) "select") (bq-select 'query));; I want to use symbols here to call functions.. look into later
	 )
      (message "sorry, not a valid statement..."))
    )
  )

(defun bq-select (bq-list)
  (dolist (bqs (get 'bq-symbol 'split-input))
    (message bqs)
    ))



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

;; (let (bbl s-test)
;;   (dolist (bbl (buffer-list))
;;     (if (cl-search "Open" (buffer-name bbl))
;; 	(progn
;; 	  (switch-to-buffer "*Buffer List*")
;; 	  (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
;; 	  (message (split-string s-test "\n"))
;; 	  ;; (move-to-window-line 2)
;; 	  ;; (Buffer-menu-mark)
;; 	  )
;;       )))
