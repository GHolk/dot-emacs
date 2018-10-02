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


(defmacro define-and-bind-text-object (name key start-regex end-regex)
  "copy and paste from stackoverflow:
https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp"
  (let* ((name-string (symbol-name name))
         (inner-name (make-symbol (concat "evil-inner-" name-string)))
         (outer-name (make-symbol (concat "evil-outer-" name-string))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (function ,inner-name))
       (define-key evil-outer-text-objects-map ,key (function ,outer-name)))))

;; kebab-case use o
;; snake_case
;; (define-and-bind-text-object SNAKE "S"
;;   "[^A-Za-z0-9_]" "[^A-Za-z0-9_]")
(progn
  (evil-define-text-object evil-inner-SNAKE
    (count &optional beg end type)
    (evil-select-paren "[^A-Za-z0-9_]" "[^A-Za-z0-9_]" beg end type count nil))
  (evil-define-text-object evil-outer-SNAKE
    (count &optional beg end type)
    (evil-select-paren "[^A-Za-z0-9_]" "[^A-Za-z0-9_]" beg end type count t))
  (define-key evil-inner-text-objects-map "S" #'evil-inner-SNAKE)
  (define-key evil-outer-text-objects-map "S" #'evil-outer-SNAKE))

;; dot.notation.chain
;; (define-and-bind-text-object dot "."
;;   "[^A-Za-z0-9_.]" "[^A-Za-z0-9_.]")
(progn
  (evil-define-text-object evil-inner-dot
    (count &optional beg end type)
    (evil-select-paren "[^A-Za-z0-9_.]" "[^A-Za-z0-9_.]" beg end type count nil))
  (evil-define-text-object evil-outer-dot
    (count &optional beg end type)
    (evil-select-paren "[^A-Za-z0-9_.]" "[^A-Za-z0-9_.]" beg end type count t))
  (define-key evil-inner-text-objects-map "." #'evil-inner-dot)
  (define-key evil-outer-text-objects-map "." #'evil-outer-dot))


(evil-mode 1)


