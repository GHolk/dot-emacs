(customize-set-variable 'evil-shift-width 4)
(customize-set-variable 'evil-find-skip-newlines t)
(customize-set-variable 'evil-want-fine-undo t)
(customize-set-variable 'evil-want-Y-yank-to-eol t)

;; eval expression in visual region
(defun eval-region-and-show (start end)
  (interactive (list (region-beginning)
                     (region-end)))
  (let ((show-result t))
    (eval-region start end show-result)))
(define-key evil-visual-state-map (kbd "C-x C-e")
  #'eval-region-and-show)

;; replace state
(setf evil-replace-state-cursor 'bar)
(define-key evil-replace-state-map
  (kbd "C-z") #'evil-normal-state)

;; insert state as emacs state
(customize-set-variable 'evil-disable-insert-state-bindings t)
(setf evil-insert-state-cursor 'hbar)
(define-key evil-insert-state-map
  (kbd "C-w") #'evil-delete-backward-word)
(define-key evil-normal-state-map
  (kbd "C-z") #'evil-insert-state)
(define-key evil-insert-state-map
  (kbd "C-z") #'evil-normal-state)
(define-key evil-insert-state-map
  (kbd "C-o") #'evil-execute-in-normal-state)
(define-key evil-insert-state-map
  (kbd "C-r") #'evil-paste-from-register)


;; customize Info mode
(customize-set-variable
 'evil-overriding-maps
 (remove-if (lambda (map-and-state)
              (eq 'Info-mode-map (car map-and-state)))
            evil-overriding-maps))
(define-key evil-motion-state-map
  (kbd "RET") nil)
(define-key evil-motion-state-map
  (kbd "TAB") nil)

;; slow move
(define-key evil-motion-state-map
  (kbd "C-f") #'evil-scroll-down)
(define-key evil-motion-state-map
  (kbd "C-b") #'evil-scroll-up)

;; for read document
(define-key evil-motion-state-map
  (kbd "SPC") #'evil-scroll-page-down)
(define-key evil-motion-state-map 
  (kbd "m") #'evil-set-marker)


;; triger text object `)` without shift
(define-key evil-outer-text-objects-map
  "0" #'evil-a-paren)
(define-key evil-inner-text-objects-map
  "0" #'evil-inner-paren)


;; bash like ex mode, remove strange ex default binding
(define-key evil-ex-completion-map
  (kbd "C-b") nil)
(define-key evil-ex-completion-map
  (kbd "C-a") nil)
(define-key evil-ex-completion-map
  (kbd "C-k") nil)
(define-key evil-ex-completion-map
  (kbd "C-d") nil)

;; allow repeat of yank command
(unless (evil-get-command-property #'evil-yank :repeat)
  (evil-set-command-property
   #'evil-yank :repeat
   (evil-get-command-property #'evil-delete :repeat)))


;; always search forward n backward N
(defun evil-search-forward-always (&optional arg pred)
  "evil use emacs default isearch, if search backward
variable isearch-forward would be set to nil,
detail function would be in evil-search-incrementally ."
  (setf isearch-forward t))

(advice-add #'evil-search-backward :after 
            #'evil-search-forward-always)
(advice-add #'evil-search-word-backward :after 
            #'evil-search-forward-always)

;; close window by `q` in evil registry `:reg`
(define-key evil-list-view-mode-map
  (kbd "q") #'quit-window)

(evil-mode 1)


