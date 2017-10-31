;; create by c34031328
;; 2016-12-10

;; el-get at top!
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(customize-set-variable 'el-get-user-package-directory
                        "~/.emacs.d/el-get-user/init")

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
    ))

(el-get nil
        '(el-get
          smex markdown-mode pangu-spacing mediawiki js2-mode
          evil evil-surround evil-args evil-numbers evil-goggles
          rainbow-delimiters js-comint))


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
(global-set-key (kbd "C-c d") #'kid-sdcv-to-buffer)

(global-set-key (kbd "C-c m") #'manual-entry)

(define-key emacs-lisp-mode-map
  (kbd "TAB") 'completion-at-point)

(set-face-attribute 'default nil :height 180)

(add-to-list 'auto-mode-alist '("/home/gholk/gholk/text/" . markdown-mode))

;; (defun assocr (images &optional tags)
;;   (interactive
;;    "sImages: 
;; sTags: ")
;;   (insert
;;    (shell-command-to-string
;;     (string-join (list
;;                   "echo"
;;                   "assocr"
;;                   (if tags
;;                       (concat "-t " tags)
;;                     "")
;;                   images)
;;                  " ")))
;; )

(defadvice ido-find-file
  (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:"
                                 buffer-file-name))))

(load-theme 'tango-dark)
