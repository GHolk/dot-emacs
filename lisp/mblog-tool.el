
(defmacro mblog-ensure-file-name (file-name)
  `(unless ,file-name
    (setf ,file-name (concat "../" (file-name-base) ".html"))))

(defvar mblog-log-buffer-name "mblog log")

(defun mblog-make-post (file-name)
  "make html in mblog markdown editing."
  (interactive "i")
  (mblog-ensure-file-name file-name)
  ;; sleep bacause:
  ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
  (let ((command-string (format "cd ../system/; make %s; sleep 1" file-name)))
    (async-shell-command command-string
                         (get-buffer-create mblog-log-buffer-name))))

(defun mblog-atom-create (file-name)
  (interactive "i")
  (mblog-ensure-file-name file-name)
  (async-shell-command
   (format "cd ../system/; ./update-atom.sh create %s" file-name)
   (get-buffer-create mblog-log-buffer-name)))

(defvar mblog-atom-append-commit-message-buffer-name "mblog commit message")
(defvar mblog-atom-append-file-name nil
  "global variable for commit use")
(defun mblog-atom-append (file-name commit-buffer)
  (interactive "i\ni")
  (mblog-ensure-file-name file-name)
  (setf mblog-atom-append-file-name file-name)
  (let ((buffer (get-buffer-create
                 mblog-atom-append-commit-message-buffer-name)))
    (switch-to-buffer buffer)
    (use-local-map (copy-keymap evil-insert-state-map))
    (local-set-key (kbd "C-x C-s")
                   #'mblog-atom-append-commit)))

(defun mblog-atom-append-commit (file-name)
  (interactive "i")
  (unless file-name
    (setf file-name mblog-atom-append-file-name))
  (cd "../system/")
  (let* ((buffer-output (get-buffer-create mblog-log-buffer-name))
         (process-connection-type nil) ; use pipe not terminal
         (process (start-process "update-atom" buffer-output
                                 "sh" "update-atom.sh" "append" file-name)))
    (display-buffer buffer-output)
    (process-send-region process (point-min) (point-max))
    (process-send-eof process)
    (accept-process-output process))
  (setf mblog-atom-append-file-name nil)
  (kill-buffer mblog-atom-append-commit-message-buffer-name))

;; (defmacro pipe (value &rest function-list)
;;   (if (null function-list)
;;       value
;;     `(pipe (,(car function-list) ,value) . ,(cdr function-list))))


(provide 'mblog-tool)
