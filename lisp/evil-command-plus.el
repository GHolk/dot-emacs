;; author: gholk
;; description: some command suit evil and emacs.

;; let ! work in text object, not work in line.
(evil-define-operator evil-shell-command-inline (beg end type)
  (if (eq type 'block)
      (evil-apply-on-block #'evil-shell-command-inline beg end nil)
    (let ((command (read-shell-command "!")))
      (if (buffer-modified-p)
          (let ((no-buffer nil)
                (replace t))
            (shell-command-on-region beg end command
                                     no-buffer replace))
    (shell-command-on-region beg end command)))))

(define-key evil-motion-state-map (kbd "!")
  #'evil-shell-command-inline)

;; eval expression in visual region
(define-key evil-visual-state-map (kbd "C-x C-e")
  #'eval-region)

;; easy define emacs region command
(defmacro define-pipe-region-command (name command)
  `(defun ,name (start end)
    (let ((replace t)
          (no-buffer nil))
      (shell-command-on-region start end
                               ,command
                               no-buffer replace))))

(defmacro define-pipe-region-command-and-evil
    (command-name evil-name command)
  `(define-pipe-region-command ,command-name ,command)
  `(evil-define-operator ,evil-name (beg end type)
     (if (eq type 'block)
         (evil-apply-on-block (function ,evil-name) beg end nil)
       (,command-name beg end))))

(define-pipe-region-command cconv-tw-region
  "cconv -f UTF8 -t UTF8-TW")
(evil-define-operator evil-cconv-tw (beg end type)
  "Convert text to traditional chinese."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-cconv-tw beg end nil)
    (cconv-tw-region beg end)))
(define-key evil-normal-state-map
  (kbd "gC") #'evil-cconv-tw)

(define-pipe-region-command cconv-cn-region
  "cconv -f UTF8 -t UTF8-CN")
(evil-define-operator evil-cconv-cn (beg end type)
  "Convert text to simply chinese."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-cconv-cn beg end nil)
    (cconv-cn-region beg end)))
(define-key evil-normal-state-map
  (kbd "gc") #'evil-cconv-cn)


(provide 'evil-command-plus)
