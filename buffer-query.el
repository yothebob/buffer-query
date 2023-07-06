(setq *buffer-query-list* (list))
(defvar bq-conditionals (list "=" "!=" "not in" "in" "like" "not like"))
(defvar buffer-columns (list "crm" "name" "size" "mode" "file"))



(defun bq-get-by-cond (&optional value q-cond &key col)
  "Given a VALUE and a COL and Q-COND, return a list of buffers that have conditions that match the value."
  (if (equal q-cond nil)
      (setq q-cond "string-equal"))
  (cl-remove-if-not #'(lambda (x) (apply (intern q-cond) (list (cl-getf x col) value))) *buffer-query-list*))
;; EX: (bq-get-by-cond "*scratch*" "string-equal" :col :name)
;; EX: (bq-get-by-cond)

(defun select (fn-list)
  ""
  (let (res)
    ;; (setq fn-list ("TODO" :col :crm))
    (setq res (apply (intern "bq-get-by-cond") fn-list)))
  ;; (apply 'bq-pretty-print res)
  )
;; EX: (select '("TODO" :col :crm))

(member "crmz" (list "a" "in" "crm"))


(defun bq-pretty-print (fields buffer)
  "Fix to print FIELDS: VALUE \n for each BUFFER."
  (message (format "%s: %s") fields buffer))


;; testing
;; (let ()
;;   (setq test-var  (intern "bq-get-by-cond"))
;;   (apply test-var '("TODO" :col :crm)))

(defun test-rest-fun (&rest args)
  (apply 'message args))
(test-rest-fun "%s %s %s" "hello" "world" "testing")


(defun bq-parse-where (command-string)
  ;; TODO tokenize command, break it down for bq-get-by-cond, then run command
  ;; (select, kill, etc..) based of res list
  (if (not (cl-search "where" command-string))
      (catch 'return
    (when t
      (throw 'return "na")))
  (let (post-where (parsed-col nil) (parsed-cond nil) (parsed-val nil))
    (progn
      (setq post-where (nth 1 (string-split command-string "where")))
      (if (cl-search "and" post-where) ;; if and in where, try to split on ands
	  (setq post-where (string-split post-where "and"))
	(setq post-where (list post-where)))
      (dolist (pw post-where)
	(dolist (pw-word (split-string pw " "))
	  (if (member pw-word buffer-columns) (setq parsed-col pw-word))
	  (if (member pw-word bq-conditionals) (setq parsed-cond pw-word))
	  ;; (if (and (member pw-word buffer-columns) (equal parsed-col nil)) (setq parsed-col pw-word))
	  ;; (if (and (member pw-word bq-conditionals) (equal parsed-cond nil)) (setq parsed-cond pw-word))
	  (if (string-equal pw-word "not") (string-join parsed-cond "not"))))
      (message (format "%s %s %s" parsed-col parsed-cond parsed-val))
    ))))

;; (bq-parse-where "select name from buffer")
;; (bq-parse-where "select name from buffer where testing like file")
;; (bq-parse-where "select name from buffer where a = file and b = name")

(defun buffer-query ()
  (interactive)
  (dolist (buff (buffer-list))
    (bq-add-to-buffer-list buff))
  (let (command-string action-word (getby-args (list)))
    (progn
      (setq command-string (downcase (read-string ":")))
      (setq action-word (nth 0 (string-split command-string " ")))
      (setq getby-args (bq-parse-where command-string))
      (if (length> getby-args 0)
	  (apply (intern "action-word") getby-args)
	(apply (intern "action-word"))))))


(defun bq-add-to-buffer-list (buffer)
  (push
   (list
    :name (buffer-name buffer)
    :size (buffer-size buffer)
    :file (if (not (equal (buffer-file-name buffer) nil)) (buffer-file-name buffer))
    :mode (buffer-local-value 'major-mode (get-buffer buffer))
    :crm "TODO")
   *buffer-query-list*))

;; for testing
;; (bq-add-to-buffer-list (get-buffer "*scratch*"))
;; (bq-add-to-buffer-list (get-buffer "*Messages*"))


;;; buffer-query.el --- A library for managing buffers in a sql like fashion  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Brandon Brodrick

;; Author: Brandon Brodrick <bbrodrick@parthenonsoftware.com>
;; Keywords: abbrev, matching, tools, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; (bq-select (bq-parse-where "select name from buffer"))
;; (bq-select (bq-parse-where "select name from buffer where .el in file"))
;; (apply (intern "select") (list (bq-parse-where "select name from buffer where .el in file")))

;;; Code:
(defvar *buffer-query-list* (list))
(defvar bq-conditionals (list "=" "!=" "not in" "in" "like" "not like"))
(defvar buffer-columns (list "crm" "name" "size" "mode" "file"))
(defvar query-history (list ""))

;;;; HELPER FUNCTIONS 

(defun string-in(value col-value)
  "Check if VALUE inside COL-VALUE returning t or nil."
  (catch 'return
    (when t
      (throw 'return (not (equal (cl-search value col-value) nil))))))

(defun get-list-remainder (from-list &rest haves)
  "From a FROM-LIST Look at all the HAVES, return the value you do not have."
  (let (unsaved (list))
  (dolist (fl from-list)
      (cond ((equal (member fl haves) nil) (push fl unsaved))))
    (catch 'return
    (when t
      (throw 'return unsaved)))))

(defun set-file-attr (buffer)
  "Handle setting file attr for BUFFER, extra logic needed magit and dired mode."
  (let (file-res)
    (cond
     ((cl-search "dired" (symbol-name (buffer-local-value 'major-mode (get-buffer buffer))))
      (with-current-buffer buffer (setq file-res default-directory)))
     ((cl-search "magit" (symbol-name (buffer-local-value 'major-mode (get-buffer buffer))))
      (with-current-buffer buffer (setq file-res default-directory)))
     ((not (equal (buffer-file-name buffer) nil)) (setq file-res (buffer-file-name buffer))))
    (catch 'return
	(when t
	  (throw 'return file-res)))))
  
(defun bq-add-to-buffer-list (buffer)
  "Add BUFFER metadata to plist."
  (push
   (list
    'name (buffer-name buffer)
    'size (buffer-size buffer)
    'file (set-file-attr buffer)
    'mode (downcase (format "%s" (buffer-local-value 'major-mode (get-buffer buffer))))
    'crm "TODO") *buffer-query-list*))

(defun clear-buffer-list ()
  "Tiny wrapper to clear buffer-list."
  (setq *buffer-query-list* (list)))

(defun bq-do-action-buffer (buffer action)
  "Do ACTION to BUFFER."
  (let (s-test (iter 0) buffer-list-line)
      (cond
       ((string-equal action "kill") (kill-buffer buffer))
       ((string-equal action "open") (open-buffer buffer))
      (t (progn
      (switch-to-buffer "*Buffer List*")
      (setq s-test (buffer-substring-no-properties (point-min) (point-max)))
      (dolist (buffer-list-line (split-string s-test "\n"))
	(if (and (cl-search (buffer-name buffer) buffer-list-line) (cl-search (number-to-string (buffer-size buffer)) buffer-list-line))
	    (progn (move-to-window-line iter)
		   (cond
		    ((string-equal action "delete") (Buffer-menu-delete))
		    ((string-equal action "mark") (Buffer-menu-mark)))))
	(setq iter (+ iter 1))))))))

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



;;;; ACTION FUNCTIONS 

(defun bq-select (fn-list)
  "Print all buffers that match FN-LIST parameters."
  (let (res)
    (setq res (apply (intern "bq-get-by-cond") fn-list))
  (message "%s" res)))

;; TODO: these can all probably be condensed to a func
(defun bq-action (fn-list action)
  "Call ACTION on buffers that match FN-LIST parameters."
  (let (res)
    (setq res (apply (intern "bq-get-by-cond") fn-list))
    (dolist (buff res)
      (bq-do-action-buffer (get-buffer (plist-get buff 'name)) action))))



;;;; LOGIC FUNCTIONS 

(defun bq-get-by-cond (&optional value q-cond &key col)
  "Given a VALUE and a COL and Q-COND, return a list of buffers that have conditions that match the value."
  (if (equal q-cond nil)
      (setq q-cond "string-equal"))
  (if (equal col nil)
      (setq col ""))
  (cl-remove-if-not #'(lambda (x) (apply (intern q-cond) (list value (plist-get x (intern col))))) *buffer-query-list*))

(defun bq-parse-where (command-string)
  "Parse COMMAND-STRING into a arg list for action-word function."
  (if (not (cl-search "where" command-string))
      (catch 'return
    (when t
      (throw 'return ())))
    (let (post-where (parsed-col nil) (parsed-cond nil) (parsed-val nil))
    (progn
      (setq post-where (nth 1 (split-string command-string "where")))
      (if (cl-search "and" post-where) ;; if and in where, try to split on ands
	  (setq post-where (split-string post-where "and"))
	(setq post-where (list post-where)))
      (dolist (pw post-where)
	(dolist (pw-word (split-string pw " "))
	  (if (member pw-word buffer-columns) (setq parsed-col pw-word))
	  (if (member pw-word bq-conditionals) (setq parsed-cond pw-word))
	  (if (string-equal pw-word "not") (string-join parsed-cond "not")))
      (cond
       ((> (length (get-list-remainder (split-string pw " ") parsed-col parsed-cond)) 0)
	(setq parsed-val (nth 0 (get-list-remainder (split-string pw " ") parsed-col parsed-cond)))))
      (message (format "%s %s %s" parsed-col parsed-cond parsed-val)))
      (cond
       ((string-equal parsed-cond "=") (setq parsed-cond "string-equal"));; todo finish adding conditionals
       ((string-equal parsed-cond "in") (setq parsed-cond "string-in")))
      (catch 'return
    (when t
      (throw 'return (list parsed-val parsed-cond :col parsed-col))))))))

;;;; USER FUNCTIONS 

(defun buffer-query ()
  "User function to query buffers based off data."
  (interactive)
  (clear-buffer-list)
  (bq--get-buffer-data)
  (dolist (buff (buffer-list))
    (bq-add-to-buffer-list buff))
  (let (command-string action-word (getby-args (list)))
    (progn
      (setq command-string (downcase (read-string ":" nil 'query-history nil)))
      (push command-string query-history)
      (setq action-word (nth 0 (split-string command-string " ")))
      ;; TODO: if action word is select, get fields to select
      (setq getby-args (bq-parse-where command-string))
      (bq-action getby-args action-word))))

(provide 'buffer-query)
;;; buffer-query.el ends here
