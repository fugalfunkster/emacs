;;; bebop-core.el --- Send Emacs text buffers to Claude tmux pane -*- lexical-binding: t; -*-

(defcustom bebop-tmux-target "claude:cli"
  "tmux target in SESSION:WINDOW form used for Claude."
  :type 'string
  :group 'bebop)

(defcustom bebop-composition-buffer-name "*bebop-compose*"
  "Buffer name used for composing Claude prompts."
  :type 'string
  :group 'bebop)

(defcustom bebop-composition-font-family "Goudy Mediaeval"
  "Font family used for the composition buffer text. Set to nil to keep default."
  :type '(choice (const :tag "Use default" nil) string)
  :group 'bebop)

(defun bebop--set-and-refresh-header (sym val)
  (set-default sym val)
  (let ((buf (get-buffer bebop-composition-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (fboundp 'bebop--set-header)
          (bebop--set-header))))))

(defcustom bebop-header-text "Bebop"
  "Header text shown at the top of the composition buffer."
  :set 'bebop--set-and-refresh-header
  :type 'string
  :group 'bebop)

(defcustom bebop-header-color "#268bd2"
  "Header text color."
  :set 'bebop--set-and-refresh-header
  :type 'string
  :group 'bebop)

(defcustom bebop-session-header-color "#dc322f"
  "Header text color for per-session composition buffers."
  :type 'string
  :group 'bebop)

(defcustom bebop-header-height 3.5
  "Header text height scale."
  :set 'bebop--set-and-refresh-header
  :type 'number
  :group 'bebop)

(defcustom bebop-header-enabled t
  "Whether to show the composition buffer header text."
  :set 'bebop--set-and-refresh-header
  :type 'boolean
  :group 'bebop)

(defface bebop-header-face
  '((t :height 3.0 :weight bold))
  "Face for the composition buffer header text."
  :group 'bebop)

(defface bebop-buffer-face
  '((t :inherit default))
  "Face for the composition buffer body text."
  :group 'bebop)

(defvar-local bebop--header-overlay nil)

(defvar-local bebop--header-mode "Bebop"
  "Current mode label shown in the header.
Set by bebop-passthrough on toggle.")

(defvar-local bebop--header-pair nil
  "Active pair name shown in the header, or nil if no pair is selected.
Set by bebop-dashboard on pair switch.")

(defun bebop--resolve-font ()
  (let* ((candidates (delq nil (list bebop-composition-font-family
                                     "Goudy Mediaeval"
                                     "Goudy Medieval")))
         (fonts (font-family-list)))
    (seq-find (lambda (name) (member name fonts)) candidates)))

(defun bebop--set-header ()
  "Show the Bebop header text as a non-editable overlay."
  (when (overlayp bebop--header-overlay)
    (delete-overlay bebop--header-overlay)
    (setq bebop--header-overlay nil))
  (when bebop-header-enabled
    (let* ((resolved (bebop--resolve-font))
           (base-h (let ((dh (face-attribute 'default :height nil t)))
                     (if (integerp dh) dh 160)))
           (abs-h (round (* bebop-header-height base-h)))
           (text (if bebop--header-pair
                     (format "%s → %s" bebop--header-mode bebop--header-pair)
                   bebop--header-mode))
           (header (propertize text
                               'face `(:height ,abs-h :weight bold
                                       :foreground ,bebop-header-color
                                       ,@(when resolved (list :family resolved)))
                               'read-only t
                               'front-sticky t
                               'rear-nonsticky t))
           (spacer (propertize "\n" 'read-only t))
           (ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'before-string (concat header spacer))
      (setq bebop--header-overlay ov))))

(defun bebop--set-header-overlay (text &optional color height)
  "Display TEXT as a styled header at point-min of the current buffer.
COLOR defaults to `bebop-header-color'; HEIGHT defaults to 3.0.
Uses an absolute height computed from HEIGHT × the default face height,
independent of any text-scale applied to the buffer."
  (when (overlayp bebop--header-overlay)
    (delete-overlay bebop--header-overlay)
    (setq bebop--header-overlay nil))
  (let* ((resolved (bebop--resolve-font))
         (c (or color bebop-header-color))
         (h (or height 3.0))
         (base-h (let ((dh (face-attribute 'default :height nil t)))
                   (if (integerp dh) dh 160)))
         (abs-h (round (* h base-h)))
         (header (propertize text
                             'face `(:height ,abs-h :weight bold
                                     :foreground ,c
                                     ,@(when resolved (list :family resolved)))
                             'read-only t
                             'front-sticky t
                             'rear-nonsticky t))
         (spacer (propertize "\n" 'read-only t))
         (ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'before-string (concat header spacer))
    (setq bebop--header-overlay ov)))

(defun bebop-refresh-header ()
  "Refresh the Bebop composition buffer header overlay."
  (interactive)
  (let ((buf (get-buffer bebop-composition-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (bebop--set-header)))))

(defun bebop--apply-buffer-settings ()
  "Apply buffer-local settings for the composition buffer."
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (boundp 'display-line-numbers)
    (setq-local display-line-numbers nil))
  (when (fboundp 'linum-mode)
    (linum-mode -1))
  ;; Scale text to 100% of the default face; line height follows automatically.
  (face-remap-add-relative 'default :height 1.0))

(defun bebop--tmux-pane-id ()
  "Return the pane id (for example, %3) for `bebop-tmux-target'."
  (let ((pane
         (string-trim
          (with-temp-buffer
            (let ((code (call-process "tmux" nil t nil
                                      "list-panes"
                                      "-t" bebop-tmux-target
                                      "-F" "#{pane_id}")))
              (unless (eq code 0)
                (error "tmux target not found: %s" bebop-tmux-target)))
            (buffer-string)))))
    (unless (string-match-p "^%[0-9]+$" pane)
      (error "Invalid pane id from tmux target %s: %s"
             bebop-tmux-target pane))
    pane))

(defun bebop-send-buffer ()
  "Send current buffer contents to Claude tmux pane, submit, then clear buffer."
  (interactive)
  (let ((pane (bebop--tmux-pane-id)))
    (call-process-region (point-min) (point-max)
                         "tmux" nil nil nil "load-buffer" "-")
    (call-process "tmux" nil nil nil "paste-buffer" "-d" "-t" pane)
    (call-process "tmux" nil nil nil "send-keys" "-t" pane "C-m")
    (erase-buffer)
    (message "Sent buffer to Claude pane %s" pane)))

(defun bebop-open-composition-buffer ()
  "Open/reuse the Claude prompt composition buffer."
  (interactive)
  (switch-to-buffer bebop-composition-buffer-name)
  (text-mode)
  (bebop--apply-buffer-settings)
  (bebop--set-header)
  (local-set-key (kbd "C-c C-c") #'bebop-send-buffer)
  (message "Compose prompt, then press C-c C-c to send to Claude"))

(defun eshell/bebop (&rest _args)
  "Eshell command to open the Bebop Conductor frame."
  (bebop)
  "")

(provide 'bebop-core)

;;; bebop-core.el ends here
