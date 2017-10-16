;; create by c34031328
;; 2016-12-10

;; el-get at top!
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s) (end-of-buffer) (eval-print-last-sexp))))

(setq
 el-get-sources
 '((:name pangu-spacing
          :after (progn
                   (customize-set-variable
                    'pangu-spacing-real-insert-separtor t)
                   ;; true add space into document
                   (global-pangu-spacing-mode 1)))
   (:name smex
          :after (progn
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name js2-mode
          :after (progn
                   (add-to-list 'auto-mode-alist
                                `(,(rx ".js" string-end) . js2-mode))
                   (customize-set-variable
                    'js2-mode-assume-strict nil)
                   (customize-set-variable
                    'js2-strict-missing-semi-warning nil)))
   (:name evil
          :after (progn
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
                   (defalias 'evil-insert-state 'evil-emacs-state
                     "use emacs state as insert state")
                   (defalias 'evil-previous-line 'evil-previous-visual-line
                     "always use visual line instead of realy line")
                   (defalias 'evil-next-line 'evil-next-visual-line
                     "always use visual line instead of realy line")
                   (customize-set-variable
                    'evil-overriding-maps
                    (mapcar (lambda (map-and-state)
                              (if (eq 'Info-mode-map (car map-and-state))
                                  '(Info-mode-map . emacs)
                                map-and-state))
                            evil-overriding-maps))
                   (evil-define-key 'motion Info-mode-map
                     (kbd "RET") 'Info-follow-nearest-node)
                   (define-key evil-motion-state-map
                     (kbd "TAB") nil)

                   (let ((key (kbd "0")))
                     (define-key evil-outer-text-objects-map
                       key 'evil-a-paren)
                     (define-key evil-inner-text-objects-map
                       key 'evil-inner-paren))

                   (define-key evil-motion-state-map
                     (kbd "SPC") 'evil-scroll-page-down)
                   (define-key evil-motion-state-map 
                     (kbd "m") 'evil-set-marker)

                   (evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)
                   (evil-define-command
                     evil-save-and-delete-buffer (file &optional bang)
                     "Saves the current buffer and only delete buffer."
                     :repeat nil
                     (interactive "<f><!>")
                     (evil-write nil nil nil file bang)
                     (evil-delete-buffer (current-buffer)))
                   (evil-ex-define-cmd "wq" 'evil-save-and-delete-buffer)

                   (define-key evil-ex-completion-map
                     (kbd "C-b") 'backward-char)
                   (define-key evil-ex-completion-map
                     (kbd "C-a") 'move-beginning-of-line)
                   (define-key evil-ex-completion-map
                     (kbd "C-k") 'kill-line)
                   (evil-mode 1)))
   
   (:name evil-surround
          :after (global-evil-surround-mode 1))
   (:name evil-args
          :after (progn
                   (define-key evil-inner-text-objects-map
                    (kbd "a") 'evil-inner-arg)
                   (define-key evil-outer-text-objects-map
                     (kbd "a") 'evil-outer-arg)))
   (:name rainbow-delimiters
          :after (progn
                   (add-hook 'prog-mode-hook
                             'nice-bracket-highligh)))
    ))

(defun nice-bracket-highligh ()
  (rainbow-delimiters-mode t)
  (show-paren-mode t))

(setq el-get-packages
      '(el-get smex markdown-mode pangu-spacing mediawiki js2-mode
               evil evil-surround evil-args
               rainbow-delimiters))

(el-get 'sync el-get-packages)

(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))


(require 'ido)
(ido-mode t)
(defun my-ido-bind-key ()
  "ido key binding need add as hook, accroding to
<https://emacs.stackexchange.com/questions/3729/how-do-i-bind-keys-in-ido>"
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match))
(add-hook 'ido-setup-hook 'my-ido-bind-key)
(global-set-key (kbd "C-x f") 'ido-find-file)

 
;;;; about Emacs itself
;; emacs has no user-agent default??
(customize-set-variable 'url-user-agent "Emacs25")
;; free most space
(menu-bar-mode -1)
(tool-bar-mode -1)
;; (customize-set-variable 'mode-line-format nil)

(customize-set-variable 'indent-tabs-mode nil)

;; let M-a M-e distinct `，；`
(customize-set-variable
 'sentence-end-without-space
 (concat sentence-end-without-space "，；")
 "break all chinese sentence")
(customize-set-variable
 'sentence-end-base
 (concat "[," (substring sentence-end-base 1))
 "break english sentence at camma.")


(customize-set-variable
 'Info-additional-directory-list
 '("/home/gholk/.local/share/info/")
 "my local info directory, always a symbol link to other directory.")

(load "~/.emacs.d/sdcv.el")
(global-set-key (kbd "C-c d") 'kid-sdcv-to-buffer)

(define-key emacs-lisp-mode-map
  (kbd "TAB") 'completion-at-point)

(set-face-attribute 'default nil :height 180)