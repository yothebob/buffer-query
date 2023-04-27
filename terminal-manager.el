
(require 'widget)
(require 'notifications)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Terminal Manager.\n\n")
  (widget-create 'menu-choice
                 :tag "Choose"
                 :value "This"
                 :help-echo "Choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "This option" :value "This")
                 '(choice-item "That option")
                 '(editable-field :menu-tag "No option" "Thus option"))
  
  (widget-insert "\nSee also ")

  (widget-insert
   " for more information.\n\nNumbers: count to three below\n")

  
  ;; (setq widget-example-repeat
  ;;       (widget-create 'editable-list
  ;;                      :entry-format "%i %d %v"
  ;;                      :notify
  ;;                      (lambda (widget &rest ignore)
  ;;                        (let ((old (widget-get widget
  ;;                                               ':example-length))
  ;;                              (new (length (widget-value widget))))
  ;;                          (unless (eq old new)
  ;;                            (widget-put widget ':example-length new)
  ;;                            (message "You can count to %d." new))))
  ;;                      :value '("One" "Eh, two?" "Five!")
  ;;                      '(editable-field :value "three")))

  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "Congratulation!")
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (use-local-map widget-keymap)
  (widget-setup))


;; TODO get all running term/shell processes. Then put them on GUI
;; (let (term-types)
;;   (setq term-types '("term" "shell" "Shell"))
;;   (dolist (pl (process-list))
;;     (cond
;;      ((cl-search (nth 0 term-types) (process-name pl)) (message "found terminal type"))
;;      ((cl-search (nth 1 term-types) (process-name pl)) (message "found shell type"))
;;      ((cl-search (nth 2 term-types) (process-name pl)) (message "found Shell type"))
;;      (t (message (format "found %s" (process-name pl)))))))



;; TODO: on terminal process finish, send desktop notification
;; (defun my-on-action-function (id key)
;;   (message "Message %d, key \"%s\" pressed" id key))

;; (defun my-on-close-function (id reason)
;;   (message "Message %d, closed due to \"%s\"" id reason))

;; (notifications-notify
;;  :title "Title"
;;  :body "This is <b>important</b>."
;;  :actions '("Confirm" "I agree" "Refuse" "I disagree")
;;  :on-action 'my-on-action-function
;;  :on-close 'my-on-close-function)


;; (process-list)
(defvar bb-process-name "BBPROCESS")
(defvar bb-process-index 0)
(defvar bb-processes '())
(defvar bb-processes-outputs (make-hash-table :test 'equal))

(defun bb-get-ran-commands-from-bb-proccess ()
  "Get the bbprocess buffer 'foo' and read all the processes/commands out of that buffer."
  (interactive)
  (let (command-res-list process-name-list iter-index)
    (progn
      (set-buffer "foo")
      (setq iter-index 0)
      (setq command-res-list (split-string (buffer-string) "Process BBPROCESS. finished"))
      (dolist (cmd command-res-list)
	(puthash (concat bb-process-name (number-to-string iter-index)) cmd bb-processes-outputs)
	(setq iter-index (+ iter-index 1))
	;; (if (not (= cmd "")))
	)
      )))

(message (string bb-processes-outputs))

(defun bb-get-ran-command-from-bb-proccess (process-label)
  "Place Holder, get process output from a PROCESS-LABEL."
  (gethash process-label bb-processes-outputs)
  )


;; (apply #'concat '("foo" "bar" "baz")) ;; apply is cool
(defun bb-make-queue-proccess ()
  "Get a command from the user, pipe that into bb-queue-proccess."
  (interactive)
  (let (user-command process-name arg-list (bb-name bb-process-name)(bb-index (number-to-string bb-process-index)))
    (progn
      (setq user-command (read-shell-command "Command: " nil))
      (setq arg-list (split-string user-command))
      (setq process-name (concat bb-name bb-index))
      (bb-queue-process process-name arg-list)))
  (push (concat bb-process-name (number-to-string bb-process-index)) bb-processes)
  (setq bb-process-index  (+ bb-process-index 1)))

(defun bb-queue-process (process-name command-list)
  "TODO: (PROCESS-NAME) (COMMAND-LIST) add optional arg to do regular 'start-process' vs 'shell-command' vs maybe eshell command?"
  (let (value)
    (apply 'start-process-shell-command
	   (append (list process-name) (list "foo") command-list))))

(defun bb-kill-process ()
  "Interactive function to kill a bbprocess with choices."
  (interactive)
  (let (found-process bb-let-processes)
    (setq bb-let-processes '())
    (dolist (pl (process-list))
      (cond
       ((cl-search "BBPROCESS" (process-name pl)) (push (process-name pl) bb-let-processes))
       (t "default")))
    (setq found-process (completing-read "Process to Kill?: " bb-let-processes))
    (kill-process found-process)))


(provide terminal-manager.el)
;;; terminal-manager.el ends here;
