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

(defun evil-keep-register-around-post-command-advice (post-command &rest args)
  "undo register reset in `evil-normal-post-command`with `:around` advice."
  (let ((register evil-this-register))
    (prog1 (apply post-command args)
      (setq evil-this-register register))))

(defun evil-keep-register-around-post-command ()
  "toggle evil-keep-register, meaning there is no default register `\"`.
the register specified with `\"` will keep working,
so you can keep append to register with `\"A`."
  (interactive)
  (let* ((body 'evil-normal-post-command)
         (advice #'evil-keep-register-around-post-command-advice)
         (enable (advice-member-p advice body)))
    (if enable
        (advice-remove body advice)
      (advice-add body :around advice))
    (message "evil-keep-register is %s" (not enable))))

(defun evil-paste-kbd-macro-advice (&rest argv)
  "make evil paste kbd-macro if register content is a macro.
this function check whether content macro by:
 1. equal to `last-kbd-macro'
 2. is a vector but not string
 3. contain unprintable character"
  (if (and (>= (length argv) 2)
           (second argv))
      (let* ((register (second argv))
             (register-pair (assoc register register-alist))
             (content (if register-pair (cdr register-pair))))
        (if (and content
                 (or (eq last-kbd-macro content)
                     (vectorp content)
                     (string-match "[^\t[:print:]\n\r]" content)))
            (let ((last-kbd-macro content))
              (forward-line)
              (beginning-of-line)
              (insert-kbd-macro '##)
              (forward-line -2)
              (search-forward "setq last-kbd-macro")
              (replace-match "execute-kbd-macro")
              t)))))
(advice-add 'evil-paste-after :before-until
            'evil-paste-kbd-macro-advice)

(evil-mode 1)


