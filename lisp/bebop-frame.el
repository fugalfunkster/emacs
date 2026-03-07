;;; bebop-frame.el --- Conductor and Solo frame layouts for Bebop -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'bebop-session)

(defun bebop ()
  "Open (or focus) the Bebop Conductor frame.

Creates a locked 3-window layout:
  Left         — chart for the active session
  Top-right    — *chant-dashboard* session pool
  Bottom-right — *bebop-session: NAME* composition buffer

Ensures the claude tmux session exists, discovers any pre-existing sessions,
and starts dashboard polling. No eshell startup ritual required."
  (interactive)
  (let ((frame (or (cl-find-if
                     (lambda (f) (frame-parameter f 'bebop-conductor))
                     (frame-list))
                   (make-frame '((name . "Bebop")
                                 (bebop-conductor . t)
                                 (fullscreen . fullscreen))))))
    (select-frame-set-input-focus frame)
    ;; Ensure tmux session, discover existing agents, start polling
    (unless (chant-dashboard--session-exists-p)
      (chant-dashboard--tmux "new-session" "-d" "-s" chant-dashboard-session))
    (chant-dashboard--discover-existing-pairs)
    (chant-dashboard--start-poll)
    ;; Set up or refresh the 3-window layout
    (bebop-frame--setup-conductor frame)))

(defun bebop-frame--setup-conductor (frame)
  "Create or refresh the locked 3-window Conductor layout in FRAME."
  (with-selected-frame frame
    (delete-other-windows)
    (let* ((total-w  (window-total-width))
           (right-w  (max 40 (/ total-w 3)))
           (left-win (selected-window))
           ;; split-window WINDOW SIZE 'right: left window gets SIZE columns
           (right-win (split-window left-win (- total-w right-w) 'right))
           ;; Split right column: dashboard (top 1/4) and composition (bottom 3/4)
           (total-h   (window-total-height right-win))
           (dash-win  right-win)
           (comp-win  (split-window right-win (max 3 (/ total-h 4)) 'below))
           (active    chant-dashboard--active-pair)
           (chart-path (and active (bebop--session-chart active)))
           (comp-buf   (and active
                            (bebop--get-or-create-composition-buffer active))))
      ;; Left: chart file
      (with-selected-window left-win
        (if (and chart-path (file-exists-p chart-path))
            (find-file chart-path)
          (dired (expand-file-name bebop-charts-dir))))
      ;; Top-right: chant-dashboard
      (with-selected-window dash-win
        (switch-to-buffer (get-buffer-create chant-dashboard-buffer-name))
        (unless (eq major-mode 'chant-dashboard-mode)
          (chant-dashboard-mode))
        (chant-dashboard--render))
      ;; Bottom-right: composition buffer
      (with-selected-window comp-win
        (if comp-buf
            (switch-to-buffer comp-buf)
          (switch-to-buffer (get-buffer-create "*bebop-composition*"))))
      ;; Weakly dedicate all three windows: switch-to-buffer is refused
      ;; interactively, but set-window-buffer still works for programmatic
      ;; updates (e.g. bebop-frame--update-conductor on session switch).
      ;; Using 'bebop (non-nil, non-t) rather than t because t = "strongly
      ;; dedicated", which blocks set-window-buffer as well.
      (set-window-dedicated-p left-win 'bebop)
      (set-window-dedicated-p dash-win 'bebop)
      (set-window-dedicated-p comp-win 'bebop)
      ;; Store references for live updates on session switch
      (set-frame-parameter frame 'bebop-chart-window left-win)
      (set-frame-parameter frame 'bebop-dashboard-window dash-win)
      (set-frame-parameter frame 'bebop-comp-window comp-win))))

(defun bebop-frame--update-conductor ()
  "Refresh chart and composition windows in the Conductor frame after a session switch.
Uses `set-window-buffer' so dedicated windows accept the buffer swap."
  (let ((frame (cl-find-if (lambda (f) (frame-parameter f 'bebop-conductor))
                            (frame-list))))
    (when frame
      (let* ((active    chant-dashboard--active-pair)
             (chart-win (frame-parameter frame 'bebop-chart-window))
             (comp-win  (frame-parameter frame 'bebop-comp-window)))
        ;; Update chart window — set-window-buffer works in dedicated windows
        (when (and chart-win (window-live-p chart-win))
          (let* ((chart-path (and active (bebop--session-chart active)))
                 (chart-buf  (if (and chart-path (file-exists-p chart-path))
                                 (find-file-noselect chart-path)
                               (dired-noselect (expand-file-name bebop-charts-dir)))))
            (set-window-buffer chart-win chart-buf)))
        ;; Update composition window and move focus there
        (when (and comp-win (window-live-p comp-win))
          (let ((comp-buf (and active
                               (bebop--get-or-create-composition-buffer active))))
            (when comp-buf
              (set-window-buffer comp-win comp-buf)
              (select-window comp-win))))))))

;; Hook into the existing session-switch mechanism in chant-dashboard
(advice-add 'chant-dashboard--apply-active-pair :after
            (lambda (&rest _) (bebop-frame--update-conductor)))

(defun bebop-solo (name)
  "Open (or focus) the Solo dev frame for session NAME.

Layout:
  Left         — treemacs rooted at the session's venue (or ~/Code)
  Center        — wide editing buffer (dired at venue root on first open)
  Right top    — eshell in the venue directory
  Right bottom — magit-status for the venue's repository"
  (interactive
   (list (completing-read "Solo session: "
                          (or (mapcar #'car bebop--sessions)
                              (user-error "No Bebop sessions exist"))
                          nil t nil nil
                          chant-dashboard--active-pair)))
  (let ((existing (cl-find-if
                    (lambda (f)
                      (equal (frame-parameter f 'bebop-solo-session) name))
                    (frame-list))))
    (if existing
        (select-frame-set-input-focus existing)
      (let ((new-frame (make-frame
                        `((name . ,(format "Solo: %s" name))
                          (bebop-solo-session . ,name)
                          (fullscreen . fullscreen)))))
        (select-frame-set-input-focus new-frame)
        (bebop-frame--setup-solo new-frame name)))))

(defun bebop-frame--setup-solo (frame name)
  "Create the Solo 3-window layout for session NAME in FRAME.
Layout: treemacs sidebar (left) | eshell (right top) | magit (right bottom).
Treemacs is allowed to create its own sidebar window; the remaining space is
split into eshell and magit. `treemacs-no-png-images' is set to t to suppress
file-type icons."
  (require 'eshell)
  (with-selected-frame frame
    (delete-other-windows)
    (let* ((venue (bebop--session-venue name))
           (root  (expand-file-name (or venue "~/Code")))
           (right-w (max 35 (/ (window-total-width) 3))))
      ;; Disable icons before treemacs opens (no-op if already set)
      (setq treemacs-no-png-images t)
      ;; Let treemacs create its own sidebar; fall back to dired split
      (if (fboundp 'treemacs)
          (let ((default-directory root))
            (treemacs)
            ;; Explicitly set treemacs project root to the session venue
            (when (fboundp 'treemacs-do-add-project-to-workspace)
              (treemacs-do-add-project-to-workspace root name)))
        (split-window (selected-window) 25 'right)
        (switch-to-buffer (dired-noselect root)))
      ;; Find the non-treemacs window for eshell/magit splitting
      (let* ((tree-win (and (fboundp 'treemacs-get-local-window)
                            (treemacs-get-local-window)))
             (edit-win (cl-find-if (lambda (w) (not (eq w tree-win)))
                                   (window-list frame)))
             (right-win (split-window edit-win
                                      (- (window-total-width edit-win) right-w)
                                      'right))
             (magit-win (split-window right-win nil 'below)))
        ;; Right top: eshell
        (with-selected-window right-win
          (let ((default-directory root))
            (eshell t)))
        ;; Right bottom: magit
        (with-selected-window magit-win
          (if (fboundp 'magit-status)
              (let ((default-directory root))
                (magit-status))
            (dired root)))
        ;; Lock sidebar windows; leave edit-win open
        (dolist (w (delq nil (list tree-win right-win magit-win)))
          (set-window-dedicated-p w t))
        (set-frame-parameter frame 'bebop-solo-center-win edit-win)))))

(defun bebop-frame--close-solo (name)
  "Close the Solo frame for session NAME, if one exists."
  (let ((frame (cl-find-if
                 (lambda (f)
                   (equal (frame-parameter f 'bebop-solo-session) name))
                 (frame-list))))
    (when frame
      (delete-frame frame))))

(provide 'bebop-frame)

;;; bebop-frame.el ends here
