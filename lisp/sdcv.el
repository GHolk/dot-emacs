(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "查字典 (默认 %s): " word)
                            nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
                                        ; 在没有创建 *sdcv* windows 之前检查是否有分屏(是否为一个 window)
                                        ; 缺憾就是不能自动开出一个小屏幕，自己注销
    (if (null (cdr (window-list)))
        (setq onewindow t)
      (setq onewindow nil))
    (let ((process (start-process-shell-command
                    "sdcv" "*sdcv*"
                    (format "sdcv -n \"%s\"" word))))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration
                   (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (customize-set-variable 'word-wrap t
               "make line wrap bueatiful")
             (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
             (local-set-key (kbd "SPC") 'scroll-up)
             (local-set-key (kbd "DEL") 'scroll-down)
             (local-set-key (kbd "q") 'quit-window)
             (evil-motion-state))
           (goto-char (point-min))))))))

(provide 'sdcv)
