;;; claude-chant.el --- Send Emacs text buffers to Claude tmux pane -*- lexical-binding: t; -*-

(defgroup claude-chant nil
  "Compose prompts in Emacs and send them to a Claude tmux pane."
  :group 'tools)

(defcustom claude-chant-target "claude:cli"
  "tmux target in SESSION:WINDOW form used for Claude."
  :type 'string
  :group 'claude-chant)

(defcustom claude-chant-buffer-name "*chant*"
  "Buffer name used for composing Claude prompts."
  :type 'string
  :group 'claude-chant)

(defcustom claude-chant-font-family "Goudy Mediaeval"
  "Font family used for the chant buffer text. Set to nil to keep default."
  :type '(choice (const :tag "Use default" nil) string)
  :group 'claude-chant)

(defun claude-chant--set-and-refresh-header (sym val)
  (set-default sym val)
  (let ((buf (get-buffer claude-chant-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (fboundp 'claude-chant--set-header)
          (claude-chant--set-header))))))

(defcustom claude-chant-header-text "Chant"
  "Header text shown at the top of the chant buffer."
  :set 'claude-chant--set-and-refresh-header
  :type 'string
  :group 'claude-chant)

(defcustom claude-chant-header-color "#268bd2"
  "Header text color."
  :set 'claude-chant--set-and-refresh-header
  :type 'string
  :group 'claude-chant)

(defcustom claude-chant-header-height 3.0
  "Header text height scale."
  :set 'claude-chant--set-and-refresh-header
  :type 'number
  :group 'claude-chant)

(defcustom claude-chant-header-enabled t
  "Whether to show the chant buffer header text."
  :set 'claude-chant--set-and-refresh-header
  :type 'boolean
  :group 'claude-chant)

(defface claude-chant-header-face
  '((t :height 3.0 :weight bold))
  "Face for the chant buffer header text."
  :group 'claude-chant)

(defface claude-chant-buffer-face
  '((t :inherit default))
  "Face for the chant buffer body text."
  :group 'claude-chant)

(defvar-local claude-chant--header-overlay nil)

(defvar-local claude-chant--header-mode "Chant"
  "Current mode label shown in the header.
Either \"Chant\" (composition) or \"Abeyance\" (passthrough).
Set by chant-abeyance on toggle.")

(defvar-local claude-chant--header-pair nil
  "Active pair name shown in the header, or nil if no pair is selected.
Set by chant-dashboard on pair switch.")

(defun claude-chant--resolve-font ()
  (let* ((candidates (delq nil (list claude-chant-font-family
                                     "Goudy Mediaeval"
                                     "Goudy Medieval")))
         (fonts (font-family-list)))
    (seq-find (lambda (name) (member name fonts)) candidates)))

(defun claude-chant--set-header ()
  "Show the chant header text as a non-editable overlay."
  (when (overlayp claude-chant--header-overlay)
    (delete-overlay claude-chant--header-overlay)
    (setq claude-chant--header-overlay nil))
  (when claude-chant-header-enabled
    (let* ((resolved (claude-chant--resolve-font))
           (_ (set-face-attribute 'claude-chant-header-face nil
                                  :height claude-chant-header-height
                                  :weight 'bold))
           (text (if claude-chant--header-pair
                     (format "%s → %s" claude-chant--header-mode claude-chant--header-pair)
                   claude-chant--header-mode))
           (header (propertize text
                               'face `(:inherit claude-chant-header-face
                                       :foreground ,claude-chant-header-color
                                       ,@(when resolved (list :family resolved)))
                               'read-only t
                               'front-sticky t
                               'rear-nonsticky t))
           (spacer (propertize "\n" 'read-only t))
           (ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'before-string (concat header spacer))
      (setq claude-chant--header-overlay ov))))

(defun claude-chant-refresh-header ()
  "Refresh the chant header overlay."
  (interactive)
  (let ((buf (get-buffer claude-chant-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-chant--set-header)))))

(defun claude-chant--apply-buffer-settings ()
  "Apply buffer-local settings for the chant buffer."
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (boundp 'display-line-numbers)
    (setq-local display-line-numbers nil))
  (when (fboundp 'linum-mode)
    (linum-mode -1)))

(defun claude-chant--pane-id ()
  "Return the pane id (for example, %3) for `claude-chant-target'."
  (let ((pane
         (string-trim
          (with-temp-buffer
            (let ((code (call-process "tmux" nil t nil
                                      "list-panes"
                                      "-t" claude-chant-target
                                      "-F" "#{pane_id}")))
              (unless (eq code 0)
                (error "tmux target not found: %s" claude-chant-target)))
            (buffer-string)))))
    (unless (string-match-p "^%[0-9]+$" pane)
      (error "Invalid pane id from tmux target %s: %s"
             claude-chant-target pane))
    pane))

(defun claude-chant-send-buffer ()
  "Send current buffer contents to Claude tmux pane, submit, then clear buffer."
  (interactive)
  (let ((pane (claude-chant--pane-id)))
    (call-process-region (point-min) (point-max)
                         "tmux" nil nil nil "load-buffer" "-")
    (call-process "tmux" nil nil nil "paste-buffer" "-d" "-t" pane)
    (call-process "tmux" nil nil nil "send-keys" "-t" pane "C-m")
    (erase-buffer)
    (message "Sent buffer to Claude pane %s" pane)))

(defun claude-chant-open-buffer ()
  "Open/reuse the Claude prompt composition buffer."
  (interactive)
  (switch-to-buffer claude-chant-buffer-name)
  (text-mode)
  (claude-chant--apply-buffer-settings)
  (claude-chant--set-header)
  (local-set-key (kbd "C-c C-c") #'claude-chant-send-buffer)
  (message "Compose prompt, then press C-c C-c to send to Claude"))

(defun eshell/chant (&rest _args)
  "Eshell command to open the chant UI.
Opens the dashboard layout if chant-dashboard is loaded, otherwise
falls back to the single-buffer composition mode."
  (if (fboundp 'chant-dashboard-open)
      (chant-dashboard-open)
    (claude-chant-open-buffer))
  "")

(provide 'claude-chant)

;;; claude-chant.el ends here
