;; 	(action-words '("select" "mark" "kill" "copy" "insert" "save" "delete")) ;; select will be like open-buffer
;; 	(conditional-words '("where" "if" "and"))
;;      (tables '("buffers"))
;;      (buffer-columns '("crm" "name" "size" "mode" "file"))

(defun bb-query-buffers (query)
  "A user function where you can manage buffers with a sql like language."
  (interactive)
  (let (q-list
	q-word
	;; q-hash
	(iter 0)
	(action-words '("select" "mark" "kill" "copy" "insert" "save" "delete")) ;; select will be like open-buffer
	(conditional-words '("where" "if" "and"))
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
  (message action query-list))

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
