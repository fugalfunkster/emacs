;;; bebop-cue.el --- Cue and jam keybindings for Bebop -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'bebop-session)

(defun bebop-cue--subtree-text ()
  "Return the full org subtree at point as a string.
Signals a user error if point is not within a heading."
  (save-excursion
    (condition-case _
        (progn
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          (let* ((beg (point))
                 (end (progn (org-end-of-subtree t t) (point))))
            (buffer-substring-no-properties beg end)))
      (error (user-error "Point is not within an org heading")))))

(defun bebop-cue--in-chart-p (session-name)
  "Return non-nil if the current buffer is SESSION-NAME's chart file."
  (let ((chart (bebop--session-chart session-name))
        (fname (buffer-file-name)))
    (and chart fname
         (string-equal (expand-file-name chart)
                       (expand-file-name fname)))))

(defun bebop-cue--active-session ()
  "Return the active session name (value of `bebop--active-session'), or nil."
  bebop--active-session)

(defun bebop-cue--pick-session ()
  "Prompt the user to select a session from all known pairs."
  (when (null bebop--live-sessions)
    (user-error "No sessions exist — create one with M-x bebop-new-session"))
  (completing-read "Session: " (mapcar #'car bebop--live-sessions) nil t))

(defun bebop-cue--require-session (&optional pick-p)
  "Return the active session name, prompting if PICK-P is non-nil or no active session."
  (if pick-p
      (bebop-cue--pick-session)
    (or (bebop-cue--active-session)
        (bebop-cue--pick-session))))

(defun bebop--jam-subtree (subtree session-name)
  "Send SUBTREE text to the tmux pane for SESSION-NAME and clear pending mode."
  (let* ((pair (cdr (assoc session-name bebop--live-sessions))))
    (unless pair
      (user-error "No dashboard entry for session: %s" session-name))
    (let ((pane-id (or (plist-get pair :pane-id)
                       (bebop--tmux-pane-id-for
                        (bebop--tmux-window-name session-name)))))
      (unless pane-id
        (user-error "No tmux pane found for session: %s (is the agent running?)"
                    session-name))
      ;; Cache the resolved pane-id
      (plist-put pair :pane-id pane-id)
      ;; load-buffer → paste-buffer → Enter (matches bebop-send-buffer)
      (with-temp-buffer
        (insert subtree)
        (call-process-region (point-min) (point-max)
                             "tmux" nil nil nil "load-buffer" "-"))
      (call-process "tmux" nil nil nil "paste-buffer" "-d" "-t" pane-id)
      (call-process "tmux" nil nil nil "send-keys" "-t" pane-id "C-m")
      (message "Jammed to session: %s" session-name))))

(defun bebop-cue ()
  "Cue: write the org subtree at point to the active session's chart."
  (interactive)
  (let* ((session (bebop-cue--require-session))
         (chart   (bebop--session-chart session))
         (subtree (bebop-cue--subtree-text)))
    (unless chart
      (user-error "Session \"%s\" has no chart" session))
    (bebop--cue-to-chart subtree chart)))

(defun bebop-cue-to ()
  "Cue: write the org subtree at point to a chosen session's chart."
  (interactive)
  (let* ((session (bebop-cue--require-session 'pick))
         (chart   (bebop--session-chart session))
         (subtree (bebop-cue--subtree-text)))
    (unless chart
      (user-error "Session \"%s\" has no chart" session))
    (bebop--cue-to-chart subtree chart)))

(defun bebop-jam ()
  "Jam: send the org subtree at point to the active session's agent via tmux.
If invoked from outside the session's chart, also writes to the chart first
so the chart remains a complete record."
  (interactive)
  (let* ((session (bebop-cue--require-session))
         (subtree (bebop-cue--subtree-text)))
    ;; Write to chart unless we are already inside it
    (unless (bebop-cue--in-chart-p session)
      (let ((chart (bebop--session-chart session)))
        (when chart
          (bebop--cue-to-chart subtree chart))))
    (bebop--jam-subtree subtree session)))

(defun bebop-jam-to ()
  "Jam: send the org subtree at point to a chosen session's agent via tmux.
If invoked from outside the session's chart, also writes to the chart first."
  (interactive)
  (let* ((session (bebop-cue--require-session 'pick))
         (subtree (bebop-cue--subtree-text)))
    (unless (bebop-cue--in-chart-p session)
      (let ((chart (bebop--session-chart session)))
        (when chart
          (bebop--cue-to-chart subtree chart))))
    (bebop--jam-subtree subtree session)))

(defvar bebop-cue-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c C-p / C-c C-P: cue to active / pick session
    ;; Intentionally shadows org-previous-visible-heading
    (define-key map (kbd "C-c C-p") #'bebop-cue)
    (define-key map (kbd "C-c C-P") #'bebop-cue-to)
    ;; C-c C-j / C-c C-J: jam to active / pick session
    ;; Intentionally shadows org-goto
    (define-key map (kbd "C-c C-j") #'bebop-jam)
    (define-key map (kbd "C-c C-J") #'bebop-jam-to)
    map)
  "Keymap for `bebop-cue-mode'.
Bindings are active in all org-mode buffers when the mode is enabled.")

(define-minor-mode bebop-cue-mode
  "Minor mode adding Bebop cue/jam keybindings to org-mode buffers.

\\{bebop-cue-mode-map}

C-c C-p — Cue subtree at point → active session's chart
C-c C-P — Cue subtree at point → pick-session chart
C-c C-j — Jam subtree at point → active session's agent
C-c C-J — Jam subtree at point → pick-session agent

Enable in all org buffers with:
  (add-hook \\='org-mode-hook #\\='bebop-cue-mode)"
  :lighter " Cue"
  :keymap bebop-cue-mode-map)

;; Enable automatically in every org-mode buffer
(add-hook 'org-mode-hook #'bebop-cue-mode)

(provide 'bebop-cue)

;;; bebop-cue.el ends here
