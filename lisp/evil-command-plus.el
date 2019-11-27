;; author: gholk
;; description: some command suit evil and emacs.


;; easy define emacs region command
(defmacro define-pipe-region-command (name command)
  `(defun ,name (start end)
    (let ((replace t)
          (no-buffer nil))
      (shell-command-on-region start end
                               ,command
                               no-buffer replace))))

;; all in one macro
(defmacro define-pipe-region-command-and-evil
    (command-name evil-name command)
  `(progn
     (define-pipe-region-command ,command-name ,command)
     (evil-define-operator ,evil-name (beg end type)
       (if (eq type 'block)
           (evil-apply-on-block (function ,evil-name) beg end nil)
         (,command-name beg end)))))

(define-pipe-region-command-and-evil
  cconv-tw-region evil-cconv-tw
  "cconv -f UTF8 -t UTF8-TW")
(define-key evil-normal-state-map
  (kbd "gt") #'evil-cconv-tw)


(define-pipe-region-command-and-evil
  cconv-cn-region evil-cconv-cn
  "cconv -f UTF8 -t UTF8-CN")
(define-key evil-normal-state-map
  (kbd "gT") #'evil-cconv-cn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custome text-object
(defmacro define-and-bind-text-object
    (inner-name outer-name key start-regex end-regex)
  "copy and paste from stackoverflow:
https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp"
  `(progn
     (evil-define-text-object ,inner-name (count &optional beg end type)
       (evil-select-paren ,start-regex ,end-regex beg end type count nil))
     (evil-define-text-object ,outer-name (count &optional beg end type)
       (evil-select-paren ,start-regex ,end-regex beg end type count t))
     (define-key evil-inner-text-objects-map ,key (function ,inner-name))
     (define-key evil-outer-text-objects-map ,key (function ,outer-name))))

;; kebab-case use o
;; path/file
(define-and-bind-text-object
  evil-inner-PATH evil-outer-PATH "/"
  "[=:\s\n;\"']" "[=:\s\n;\"']")

;; snake_case
(define-and-bind-text-object
  evil-inner-SNAKE evil-outer-SNAKE "S"
  "[^A-Za-z0-9_]" "[^A-Za-z0-9_]")

;; dot.notation.chain
(define-and-bind-text-object
  evil-inner-DOT evil-outer-DOT "."
  "[^A-Za-z0-9_.]" "[^A-Za-z0-9_.]")


;; fix insert state cursor over head a character
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

(define-key evil-motion-state-map
  (kbd "M-e") #'force-forward-sentence)


;; let `:q` kill buffer, just like vi
(evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)
(evil-define-command
  evil-save-and-delete-buffer (file &optional bang)
  "Saves the current buffer and only delete buffer."
  :repeat nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang)
  (evil-delete-buffer (current-buffer)))
(evil-ex-define-cmd "wq" #'evil-save-and-delete-buffer)


;; easy define command throuth existing elisp function
(defmacro evil-define-replace-string
    (evil-command-name function-name)
  "define a command take select text as function input
and replace select text with function output."
  `(evil-define-operator ,evil-command-name (beg end type)
     (if (eq type 'block)
         (evil-apply-on-block (function ,evil-command-name) beg end nil)
       (let* ((input (buffer-substring-no-properties beg end))
              (output (funcall ,function-name input)))
         (if buffer-read-only
             (message input)
           (progn (delete-region beg end)
                  (insert output)))))))

;; url encode decode
(defun url-unhex-string-decode-utf8 (code)
  "decode url and decode binary to utf8."
  (-> code (url-unhex-string 'allow-newlines)
           (decode-coding-string 'utf-8)))
(evil-define-replace-string evil-url-encode #'url-hexify-string)
(define-key evil-motion-state-map "gp" #'evil-url-encode)
(evil-define-replace-string evil-url-decode #'url-unhex-string-decode-utf8)
(define-key evil-motion-state-map "gP" #'evil-url-decode)

;; let ! work in text object, not work in line.
(evil-define-operator evil-shell-command-inline (beg end type)
  (if (eq type 'block)
      (evil-apply-on-block #'evil-shell-command-inline beg end nil)
    (let ((command (read-shell-command "!")))
      (if buffer-read-only
          (shell-command-on-region beg end command)
        (let ((no-buffer nil)
              (replace t))
          (shell-command-on-region beg end command
                                   no-buffer replace))))))

(define-key evil-motion-state-map (kbd "!")
  #'evil-shell-command-inline)

;; move over text object
(evil-define-motion evil-forward-text-object
  (count &optional text-object)
  "move to the end of following input text-object define 
in evil-inner-text-objects-map ."
  (unless text-object
      (setf text-object
            (let ((key (read-key-sequence "text-object:")))
              (lookup-key evil-inner-text-objects-map key))))
  (let* ((region (funcall text-object count))
         (end (nth 1 region)))
    (goto-char end)))
(define-key evil-motion-state-map (kbd "M-w")
  #'evil-forward-text-object)

(evil-define-motion evil-backward-text-object
  (count &optional text-object)
  "move to the begin of following input text-object define 
in evil-inner-text-objects-map ."
  (unless text-object
      (setf text-object
            (let ((key (read-key-sequence "text-object:")))
              (lookup-key evil-inner-text-objects-map key))))
  (let* ((region (funcall text-object count))
         (start (nth 0 region)))
    (goto-char start)))
(define-key evil-motion-state-map (kbd "M-b")
  #'evil-backward-text-object)

;; provide feature name prevent re include
(provide 'evil-command-plus)
