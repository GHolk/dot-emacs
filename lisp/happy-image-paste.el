;; allow paste and drop image from firefox/x-window

;;;; todo
;; drop file text/uri-list

(require 'dash)

(defun happy-image-paste ()
  "save jpeg image in clipboard to local file,
and insert markdown format image;
finally move cursor to input alt text.
return nil if no image data in clipboard."
  (interactive)
  (let ((clipboard-type (gui-backend-get-selection 'CLIPBOARD 'TARGETS)))
    (if (seq-position clipboard-type 'image/jpeg)
        (happy-image--save-file
         (gui-backend-get-selection 'CLIPBOARD 'image/jpeg) "jpg"))))

(defun happy-image-remove-file (&optional confirm path)
  "remove image around cursor from hard drive.
if confirm is nil or 'prompt, prompt confirm before delete.
if path is nil, get path from cursor position."
  (interactive)
  (unless path
    (require 'thingatpt)
    (setf path (thing-at-point 'filename 'no-text-property)))
  (if (or (and confirm (not (eq confirm 'prompt)))
          (y-or-n-p (format "delete %s?" path)))
      (delete-file path 'try-trash-can)))

(defun happy-image--save-file (data extension)
  "save data to local file with random name and specify extionsion"
  (let* ((temp-file-name
          (let ((coding-system-for-write 'raw-text)
                (buffer-file-coding-system 'raw-text))
            (make-temp-file "img" nil extension data)))
         (new-name (happy-image-populate-path temp-file-name extension)))
    (rename-file temp-file-name new-name)
    (happy-image-insert-link new-name)))

(defcustom happy-image-insert-link-before-cursor "\n!["
  "text to insert before cursor, will called with
(format happy-image-insert-link-before-cursor link-path)")
(defcustom happy-image-insert-link-after-cursor "](%s)\n"
  "text to insert before cursor, will called with
(format happy-image-insert-link-after-cursor link-path)")
(defun happy-image-insert-link (path)
  "insert path as image and move cursor to alt"
  (insert (format happy-image-insert-link-before-cursor path))
  (let ((text-after (format happy-image-insert-link-after-cursor path)))
    (insert text-after)
    (backward-char (length text-after))))

;; firefox drop image
;; (text/x-moz-url _NETSCAPE_URL text/x-moz-url-data text/x-moz-url-desc application/x-moz-custom-clipdata text/_moz_htmlcontext text/_moz_htmlinfo text/html text/unicode text/plain;charset=utf-8 text/plain application/x-moz-nativeimage application/x-moz-file-promise XdndDirectSave0 application/x-moz-file-promise-url application/x-moz-file-promise-dest-filename)
;; file explorer
;; (text/uri-list  )

(customize-set-value 'x-dnd-known-types
                     (cons "text/html" x-dnd-known-types))
(customize-set-value 'x-dnd-types-alist
                     (cons '("text/html" . happy-image-insert-from-html)
                           x-dnd-types-alist))

(defun happy-image-insert-from-html (window action html)
  (let ((url (with-temp-buffer
               (x-dnd-insert-utf16-text window action html)
               (let ((max-mini-window-height 0))
                 (shell-command-on-region (point-min) (point-max)
                                          "jquery -e \"\\$('img').attr('src')\""
                                          'directly-current-buffer
                                          'replace-region))
               (re-search-forward "\n")
               (replace-match "")
               (buffer-string))))
    (if (string-match "tp" url)
        (happy-image-insert-link (happy-image-url-to-local url)))))

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
      (if (string-match "\\(jpe?g\\|png\\|gif\\|webm\\)$" extension)
          (happy-image-download-safe url base extension)
        (happy-image-download-safe url nil nil)))))

(defun happy-image-download-safe (url base extension)
  "download url to filename.
if base are not specified, use random name.
if extension not specify, determine extension.
if file already exist, make a new name."
  (let ((buffer-origin (current-buffer)))
  (with-temp-buffer ; (url-retrieve-synchronously 'silent 'no-cookie)))
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
                          (if (string= extension "jpeg")
                              (setf extension "jpg"))
                          (delete-region (point-min) (1+ point-header-end)))
                 (progn (erase-buffer)
                        (error "url is not image"))))))
    (let ((path (with-current-buffer buffer-origin
                  (happy-image-populate-path base extension)))
          (coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) path)
      (delete-other-windows)
      path))))

(defun happy-image-populate-path (base extension)
  "populate a file name that will not overwrite existing file"
  (let ((dir (happy-image-get-image-directory)))
    (format "%s/%s.%s" dir (happy-image-name-from-date) extension)))

(defcustom happy-image-directory-alist
  '(("~/.*" . "~/Downloads")
    ("/.*" . "/tmp"))
  "regexp of file path and its directory to store drop/paste image.
when matching, the string will be surrounded by \"^$\"")

(defun happy-image-get-image-directory (&optional file-path)
  (if (null file-path)
      (setf file-path
            (or (buffer-file-name)
                (expand-file-name "~"))))
  (--> happy-image-directory-alist
   (seq-find (lambda (pair)
               (string-match (->> pair (car)
                                 (expand-file-name)
                                 (format "^%s$"))
                             file-path))
             it)
   (if it (cdr it))))

(defun happy-image-name-from-date ()
  (format-time-string "%F-%H-%M-%S"))

(provide 'happy-image-paste)
