(defun nice-bracket-highligh ()
  (rainbow-delimiters-mode t)
  (show-paren-mode t))

(add-hook 'prog-mode-hook
          #'nice-bracket-highligh)
