;; create by c34031328
;; 2016-12-10

;; el-get at top!
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
      (url-retrieve
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
       (lambda (s)
         (end-of-buffer)
         (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path
             "~/.emacs.d/el-get-user/recipes")
(customize-set-variable 'el-get-user-package-directory
                        "~/.emacs.d/el-get-user/init")

(el-get nil '(el-get
              smex markdown-mode pangu-spacing js2-mode
              evil evil-surround evil-args evil-numbers evil-goggles
              rainbow-delimiters js-comint vimish-fold))



;; fix package list error
;; https://github.com/syl20bnr/spacemacs/issues/3854
(load-library "url-handlers") 

(require 'package)
(setf package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; my own library
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'evil-little-word)
(require 'evil-command-plus)

(defun disable-pangu-mode ()
  (pangu-spacing-mode -1))
(add-hook 'nxml-mode-hook #'disable-pangu-mode)

(require 'ido)
(ido-mode t)
(defun my-ido-bind-key ()
  "ido key binding need add as hook, accroding to
<https://emacs.stackexchange.com/questions/3729/how-do-i-bind-keys-in-ido>"
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match))
(add-hook 'ido-setup-hook #'my-ido-bind-key)
(global-set-key (kbd "C-x f") 'ido-find-file)

;; hint elisp function
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(which-function-mode 1)
(evil-commentary-mode 1)
(evil-vimish-fold-mode 1)
 
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
(global-set-key (kbd "C-c d") #'kid-sdcv-to-buffer)

(global-set-key (kbd "C-c m") #'manual-entry)

(define-key emacs-lisp-mode-map
  (kbd "TAB") 'completion-at-point)

(set-face-attribute 'default nil :height 130)

(add-to-list 'auto-mode-alist '("/home/gholk/gholk/text/" . markdown-mode))


(defadvice ido-find-file
  (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:"
                                 buffer-file-name))))


;; send to selection if ok
(customize-set-variable
 'select-enable-primary t)

(load-theme 'tango-dark)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/home/gholk/.local/share/info/")) t)
 '(el-get-user-package-directory "~/.emacs.d/el-get-user/init")
 '(evil-find-skip-newlines t t)
 '(evil-goggles-duration 0.1)
 '(evil-overriding-maps
   (quote
    ((Buffer-menu-mode-map)
     (color-theme-mode-map)
     (comint-mode-map)
     (compilation-mode-map)
     (grep-mode-map)
     (dictionary-mode-map)
     (ert-results-mode-map . motion)
     (speedbar-key-map)
     (speedbar-file-key-map)
     (speedbar-buffers-key-map))))
 '(evil-shift-width 4)
 '(evil-want-Y-yank-to-eol t)
 '(evil-want-fine-undo t)
 '(indent-tabs-mode nil)
 '(js2-mode-assume-strict nil t)
 '(js2-strict-missing-semi-warning nil t)
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (## "evil-vimish-fold" evil-vimish-fold rainbow-mode)))
 '(pangu-spacing-real-insert-separtor t)
 '(select-enable-primary t)
 '(sentence-end-base "[,.?!…‽][]\"'”’)}]*")
 '(sentence-end-without-space "。．？！，；")
 '(url-user-agent "Emacs25"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-changed ((t (:background "dim gray"))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
