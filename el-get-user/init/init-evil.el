(customize-set-variable 'evil-shift-width 4)
(customize-set-variable 'evil-find-skip-newlines t)
(customize-set-variable 'evil-want-fine-undo t)
(customize-set-variable 'evil-want-Y-yank-to-eol t)

(defun force-forward-sentence (&optional arg)
  "fix can not forward sentence 
when sentence end at eol in evil."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let* ((before (point))
         (move (forward-sentence arg))
         (after (point)))
    (if (>= 1 (- after before))
        (forward-sentence arg))))

(define-key evil-replace-state-map
  (kbd "C-z") #'evil-normal-state)
(define-key evil-motion-state-map
  (kbd "M-e") #'force-forward-sentence)
(defalias #'evil-insert-state #'evil-emacs-state
  "use emacs state as insert state")
(define-key evil-emacs-state-map
  (kbd "C-o") #'evil-execute-in-normal-state)
;; (define-key evil-emacs-state-map
;;   (kbd "ESC") #'evil-exit-emacs-state)
(defalias #'evil-previous-line #'evil-previous-visual-line
  "always use visual line instead of realy line")
(defalias #'evil-next-line #'evil-next-visual-line
  "always use visual line instead of realy line")

;; customize Info mode
(customize-set-variable
 'evil-overriding-maps
 (remove-if (lambda (map-and-state)
              (eq 'Info-mode-map (car map-and-state)))
            evil-overriding-maps))
(evil-define-key 'motion Info-mode-map
  (kbd "RET") #'Info-follow-nearest-node)
(define-key evil-motion-state-map
  (kbd "TAB") nil)

(let ((key (kbd "0")))
  (define-key evil-outer-text-objects-map
    key #'evil-a-paren)
  (define-key evil-inner-text-objects-map
    key #'evil-inner-paren))

(define-key evil-motion-state-map
  (kbd "SPC") #'evil-scroll-page-down)
(define-key evil-motion-state-map 
  (kbd "m") #'evil-set-marker)

(evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)
(evil-define-command
  evil-save-and-delete-buffer (file &optional bang)
  "Saves the current buffer and only delete buffer."
  :repeat nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang)
  (evil-delete-buffer (current-buffer)))
(evil-ex-define-cmd "wq" #'evil-save-and-delete-buffer)

(define-key evil-ex-completion-map
  (kbd "C-b") #'backward-char)
(define-key evil-ex-completion-map
  (kbd "C-a") #'move-beginning-of-line)
(define-key evil-ex-completion-map
  (kbd "C-k") #'kill-line)
(define-key evil-ex-completion-map
  (kbd "C-d") nil)

(evil-mode 1)

(evil-vimish-fold 1)
