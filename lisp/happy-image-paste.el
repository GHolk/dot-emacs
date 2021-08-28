;; allow paste and drop image from firefox/x-window

(require 'dash)

(defun happy-image-paste ()
  "save jpeg image in clipboard to local file,
and insert markdown format image; 
finally move cursor to input alt text."
  (interactive)
  (let ((clipboard-type (gui-backend-get-selection 'CLIPBOARD 'TARGETS)))
    (if (seq-position clipboard-type 'image/jpeg)
        (happy-image--save-file
         (gui-backend-get-selection 'CLIPBOARD 'image/jpeg) ".jpeg")
      (error "no jpeg image in clipboard"))))

(defun happy-image--save-file (data extension)
  "save data to local file with random name and specify extionsion"
  (let* ((image-directory "./img/")
         (temp-file-name
          (let ((coding-system-for-write 'raw-text)
                (buffer-file-coding-system 'raw-text))
            (make-directory image-directory t)
            (make-temp-file "img" nil extension data)))
         (file-name (replace-regexp-in-string "\\." ""
                                              (format "%s" (float-time))))
         (new-name (concat image-directory  file-name  extension)))
    (rename-file temp-file-name new-name)
    (happy-image-insert-markdown-then-title new-name)))

(defun happy-image-insert-markdown-then-title (path)
  "insert path as image and move cursor to alt"
  (insert-before-markers "![")
  (insert (format "](%s)" path))
  )
    
;;;; not work
;; (defun my-dnd-func (event)
;;   (interactive "e")
;;   (message "my-dnd-func")
;;   (goto-char (nth 1 (event-start event)))
;;   (x-focus-frame nil)
;;   (setf event-current event)
;;   (let* ((payload (car (last event)))
;;          (type (car payload))
;;          (fname (cadr payload))
;;          (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
;;     (cond
;;      ;; insert image link
;;      ((and  (eq 'drag-n-drop (car event))
;;             (eq 'file type)
;;             (string-match img-regexp fname))
;;       (insert (format "![](%s)" fname))
;;       (org-display-inline-images t t))
;;      ;; insert image link with caption
;;      ((and  (eq 'C-drag-n-drop (car event))
;;             (eq 'file type)
;;             (string-match img-regexp fname))
;;       (insert "#+ATTR_ORG: :width 300\n")
;;       (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
;;       (insert (format "[[%s]]" fname))
;;       (org-display-inline-images t t))
;;      ;; C-drag-n-drop to open a file
;;      ((and  (eq 'C-drag-n-drop (car event))
;;             (eq 'file type))
;;       (find-file fname))
;;      ((and (eq 'M-drag-n-drop (car event))
;;            (eq 'file type))
;;       (insert (format "[[attachfile:%s]]" fname)))
;;      ;; regular drag and drop on file
;;      ((eq 'file type)
;;       (insert (format "[[%s]]\n" fname)))
;;      (t
;;       (error "I am not equipped for dnd on %s" payload)))))
;;
;; (define-key evil-normal-state-map (kbd "<drag-n-drop>") 'my-dnd-func)

;; firefox drop image
;; (text/x-moz-url _NETSCAPE_URL text/x-moz-url-data text/x-moz-url-desc application/x-moz-custom-clipdata text/_moz_htmlcontext text/_moz_htmlinfo text/html text/unicode text/plain;charset=utf-8 text/plain application/x-moz-nativeimage application/x-moz-file-promise XdndDirectSave0 application/x-moz-file-promise-url application/x-moz-file-promise-dest-filename)
;; file explorer
;; (text/uri-list  )


;; x-dnd-known-types
;; x-dnd-types-alist

(customize-set-value 'x-dnd-known-types
                     (cons "text/html" x-dnd-known-types))
(customize-set-value 'x-dnd-types-alist
                     (cons '("text/html" . happy-image-insert-from-html)
                           x-dnd-types-alist))

;; (defun x-dnd-save-file-insert-link (window _action url)
;;   ""
;;   (insert (format "![](%s)" url)))

(defun happy-image-insert-from-html (window action html)
  (let ((url (with-temp-buffer
               (x-dnd-insert-utf16-text window action html)
               (let ((max-mini-window-height 0))
                 (shell-command-on-region (point-min) (point-max)
                                          "jquery -e \"\\$('img').attr('src')\""
                                          (current-buffer)))
               (re-search-forward "\n")
               (replace-match "")
               (buffer-string))))
    (insert (format "\n![](%s)\n" (happy-image-url-to-local url)))
    (search-backward "]")))

(defun happy-image-url-to-local (url)
  "download image from url, naming it properly,
and return the local name."
  (require 'url-parse)
  (let ((file-name (-> url (url-generic-parse-url)
                       (url-path-and-query)
                       (car)
                       (file-name-nondirectory))))
    (let ((case-fold-search t)
          (base (file-name-base file-name))
          (extension (file-name-extension file-name)))
      (if (string-match "jpe?g\\|png\\|gif\\|webm" extension)
          (happy-image-download-safe url base extension)
        (happy-image-download-safe url nil nil)))))

(defun happy-image-download-safe (url base extension)
  "download url to filename. 
if base are not specified, use random name.
if extension not specify, determine extension.
if file already exist, make a new name."
  (with-current-buffer ; (url-retrieve-synchronously 'silent 'no-cookie)))
      (get-buffer-create "*Happy Image Download*")
    (if extension
        (call-process "wget" nil (current-buffer) nil "--quiet" "-O" "-" url)
      (progn (call-process "wget" nil (current-buffer) nil
                           "-O" "-" "--quiet" "--save-headers" url)
             (let ((case-fold-search t)
                   (point-header-end 0))
               (goto-char (point-min))
               (re-search-forward "^\r$" nil 'no-error)
               (setf point-header-end (point))
               (if (re-search-backward "^content-type: image/\\(.*\\)\r"
                                       nil 'no-error)
                   (progn (setf extension (match-string 1))
                          (delete-region (point-min) (1+ point-header-end)))
                 (progn (erase-buffer)
                        (error "url is not image"))))))
    (let ((path (happy-image-populate-path base extension))
          (coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) path)
      (erase-buffer)
      (delete-other-windows)
      path)))

(defun happy-image-populate-path (base extension)
  "populate a file name that will not overwrite existing file"
  (let ((dir "./img")
        (repeat 1))
    (unless base
      (setf base (format-time-string "%F-%H-%M-%S")))
    (while (file-exists-p (concat dir "/" base "." extension))
      (if (string-match "\\(.*\\)-\\([0-9]+\\)$" base)
          (setf base (format "%s-%d"
                             (match-string 1 base)
                             (-> (match-string 2 base)
                                 (string-to-number)
                                 (1+))))
        (setf base (concat base "-1"))))
    (concat dir "/" base "." extension)))

