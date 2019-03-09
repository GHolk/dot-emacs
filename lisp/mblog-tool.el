
(defmacro mblog-ensure-file-name (file-name)
  `(unless ,file-name
    (setf ,file-name (concat "../" (file-name-base) ".html"))))

(defun mblog-make-post (file-name)
  "make html in mblog markdown editing."
  (interactive "i")
  (mblog-ensure-file-name file-name)
  ;; sleep bacause:
  ;; https://askubuntu.com/questions/646631/emacs-doesnot-work-with-xdg-open
  (let ((command-string (format "cd ../system/; make %s; sleep 1" file-name)))
    (async-shell-command command-string)))

(defun mblog-update-atom (action file-name)
  )

(defun mblog-atom-create (file-name)
  (interactive "i")
  (mblog-ensure-file-name file-name)
  (async-shell-command
   (format "cd ../system/; ./update-atom.sh create %s" file-name)))

;; (defun mblog-atom-append (file-name commit-buffer)
;;   (interactive "i\ni")
;;   (mblog-ensure-file-name file-name)
;;   (get-buffer-create "mblog-commit-message")
;;   (async-shell-command
;;    (format "cd ../system/; ./update-atom.sh append %s" file-name)))
    
;; (defun mblog-test-buffer ()
;;   (interactive)
;;   (let ((file-name "foo.bar")
;;         (message-buffer (get-buffer-create "mblog-message")))
;;     (switch-to-buffer message-buffer)
;;     (use-local-map (copy-keymap evil-normal-state-map))
;;     (local-set-key (kbd "C-x C-s")
;;                      #'(lambda ()
;;                          (erase-buffer)
;;                          (insert file-name)))))

(provide 'mblog-tool)
