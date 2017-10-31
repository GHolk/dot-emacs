(add-to-list 'auto-mode-alist
             `(,(rx ".js" string-end) . js2-mode))
(customize-set-variable
 'js2-mode-assume-strict nil)
(customize-set-variable
 'js2-strict-missing-semi-warning nil)
