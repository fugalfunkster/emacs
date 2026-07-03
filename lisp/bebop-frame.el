;;; bebop-frame.el --- Conductor and Solo frame layouts for Bebop -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'bebop-session)

(defun bebop-frame--most-recent-chart ()
  "Return the path to the most recently modified chart in `bebop-charts-dir', or nil."
  (let* ((dir (expand-file-name bebop-charts-dir))
         (files (when (file-directory-p dir)
                  (directory-files dir t "\\.org\\'" t))))
    (when files
      (car (sort files
                 (lambda (a b)
                   (time-less-p
                    (file-attribute-modification-time (file-attributes b))
                    (file-attribute-modification-time (file-attributes a)))))))))

(defun bebop-frame--get-or-create-eshell (name dir)
  "Return the *eshell: NAME* buffer, creating it at DIR if absent."
  (or (get-buffer (format "*eshell: %s*" name))
      (let ((default-directory (expand-file-name dir)))
        (let ((buf (eshell t)))
          (with-current-buffer buf
            (rename-buffer (format "*eshell: %s*" name) t))
          buf))))

(defun bebop ()
  "Open (or focus) the Bebop Conductor frame.

Creates a locked 3-window layout (two columns):
  Left 40%  — top 50%  : *bebop* session pool dashboard
              bottom 50%: four eshell tabs (org, charts, repos, venues)
  Right 60% — *bebop-session: NAME* composition buffer (2× font)

Ensures the claude tmux session exists, discovers any pre-existing sessions,
and starts dashboard polling. No eshell startup ritual required."
  (interactive)
  (let ((frame (or (cl-find-if
                     (lambda (f) (frame-parameter f 'bebop-conductor))
                     (frame-list))
                   (make-frame '((name . "Bebop")
                                 (bebop-conductor . t)
                                 (fullscreen . maximized))))))
    (select-frame-set-input-focus frame)
    ;; Ensure tmux session, discover existing agents, start polling
    (unless (bebop--tmux-session-exists-p)
      (bebop--tmux "new-session" "-d" "-s" bebop-tmux-session))
    (bebop--discover-existing-sessions)
    (bebop--start-poll)
    ;; Set up or refresh the 3-window layout
    (bebop-frame--setup-conductor frame)))

(defun bebop-frame--setup-conductor (frame)
  "Create or refresh the locked 2-column Conductor layout in FRAME.

Left column (40% width):
  top 50%  — *bebop* dashboard
  bottom 50% — two eshell buffers with tab-line tabs (org and repos)

Right column (60% width):
  *bebop-session: NAME* composition buffer at 2× font size"
  (with-selected-frame frame
    (delete-other-windows)
    (let* ((total-w   (window-total-width))
           (main-win  (selected-window))
           ;; split-window SIZE 'right gives SIZE to the existing (left) window
           (right-win (split-window main-win (max 20 (/ (* total-w 2) 5)) 'right))
           (left-win  main-win)
           ;; Split left: dashboard top 50%, eshell bottom 50%
           (total-h   (window-total-height left-win))
           (dash-win  left-win)
           (shell-win (split-window left-win (max 3 (/ total-h 2)) 'below))
           (active    bebop--active-session)
           (comp-buf  (and active
                           (bebop--get-or-create-composition-buffer active))))
      ;; Top-left: *bebop* dashboard
      (with-selected-window dash-win
        (switch-to-buffer (get-buffer-create bebop-buffer-name))
        (unless (eq major-mode 'bebop-dashboard-mode)
          (bebop-dashboard-mode))
        (bebop--render))
      ;; Bottom-left: four eshell buffers with named tab-line tabs.
      ;; Reuses existing *eshell: LABEL* buffers if present — avoids accumulating
      ;; stale <2>, <3> copies on each tangle-and-reload.
      ;; tab-line-mode enabled locally only — Solo and standalone eshells unaffected.
      (let (org-buf charts-buf repos-buf venues-buf)
        ;; Wrap in with-selected-window so the eshell advice (which uses
        ;; selected-window to place new buffers) always targets shell-win.
        (with-selected-window shell-win
          (setq venues-buf (bebop-frame--get-or-create-eshell "venues" bebop-venues-dir))
          (setq repos-buf  (bebop-frame--get-or-create-eshell "repos"  bebop-repos-dir))
          (setq charts-buf (bebop-frame--get-or-create-eshell "charts" bebop-charts-dir))
          (setq org-buf    (bebop-frame--get-or-create-eshell "org"    bebop-org-dir)))
        (set-window-buffer shell-win org-buf)
        (let ((tabs-fn (lambda ()
                         (seq-filter #'buffer-live-p
                                     (list org-buf charts-buf repos-buf venues-buf)))))
          (dolist (buf (list org-buf charts-buf repos-buf venues-buf))
            (with-current-buffer buf
              (setq-local tab-line-tabs-function tabs-fn)
              (tab-line-mode 1)))))
      ;; Right: composition buffer at 2× font size (text-scale step 4 ≈ 2.07×)
      (with-selected-window right-win
        (switch-to-buffer (or comp-buf (get-buffer-create "*bebop-composition*")))
        (text-scale-set 3))
      ;; Weakly dedicate all three windows
      (set-window-dedicated-p dash-win  'bebop)
      (set-window-dedicated-p shell-win 'bebop)
      (set-window-dedicated-p right-win 'bebop)
      ;; Store references for live updates on session switch
      (set-frame-parameter frame 'bebop-chart-window    nil)
      (set-frame-parameter frame 'bebop-dashboard-window dash-win)
      (set-frame-parameter frame 'bebop-comp-window     right-win)
      ;; Frame-local window divider styling — Conductor only, Solos and other
      ;; frames keep the global settings.
      ;; First/last pixels match the background (invisible gutters);
      ;; middle pixel is black (a single subtle hairline between panes).
      (let ((bg (face-attribute 'default :background nil t)))
        (set-face-attribute 'window-divider             frame :foreground (face-attribute 'shadow :foreground nil t))
        (set-face-attribute 'window-divider-first-pixel frame :foreground bg)
        (set-face-attribute 'window-divider-last-pixel  frame :foreground bg)
        (set-face-attribute 'fringe                     frame :background bg)
        (set-face-attribute 'internal-border            frame :background bg)))))

(defun bebop-frame--update-conductor ()
  "Refresh chart and composition windows in the Conductor frame after a session switch.
Uses `set-window-buffer' so dedicated windows accept the buffer swap."
  (let ((frame (cl-find-if (lambda (f) (frame-parameter f 'bebop-conductor))
                            (frame-list))))
    (when frame
      (save-selected-window
        (let* ((active    bebop--active-session)
               (chart-win (frame-parameter frame 'bebop-chart-window))
               (comp-win  (frame-parameter frame 'bebop-comp-window)))
          ;; Update chart window only when the active session has a chart.
          ;; If no chart, leave the pane unchanged.
          (when (and chart-win (window-live-p chart-win))
            (let* ((chart-path (and active (bebop--session-chart active)))
                   (chart-buf  (when (and chart-path (file-exists-p chart-path))
                                 (find-file-noselect chart-path))))
              (when chart-buf
                (set-window-buffer chart-win chart-buf))))
          ;; Update composition window without stealing focus.
          (when (and comp-win (window-live-p comp-win))
            (let ((comp-buf (and active
                                 (bebop--get-or-create-composition-buffer active))))
              (when comp-buf
                (set-window-buffer comp-win comp-buf)
                (with-selected-window comp-win
                  (text-scale-set 3))))))))))

;; Hook into the existing session-switch mechanism in bebop-dashboard
(advice-add 'bebop--apply-active-session :after
            (lambda (&rest _) (bebop-frame--update-conductor)))

(defun bebop-solo (name)
  "Open (or focus) the Solo dev frame for session NAME.

Layout:
  Left  — eshell at the venue directory (or ~/Code if no venue)
  Right — magit-status for the venue's repository"
  (interactive
   (list (completing-read "Solo session: "
                          (or (mapcar #'car bebop--sessions)
                              (user-error "No Bebop sessions exist"))
                          nil t nil nil
                          bebop--active-session)))
  (let ((existing (cl-find-if
                    (lambda (f)
                      (equal (frame-parameter f 'bebop-solo-session) name))
                    (frame-list))))
    (if existing
        (select-frame-set-input-focus existing)
      (let ((new-frame (make-frame
                        `((name . ,(format "Solo: %s" name))
                          (bebop-solo-session . ,name)
                          (fullscreen . maximized)))))
        (select-frame-set-input-focus new-frame)
        (bebop-frame--setup-solo new-frame name)))))

(defun bebop-frame--setup-solo (frame name)
  "Create the Solo 2-column layout for session NAME in FRAME.
Layout: eshell at venue cwd (left) | magit-status (right)."
  (require 'eshell)
  (with-selected-frame frame
    (delete-other-windows)
    (let* ((venue (bebop--session-venue name))
           (root  (expand-file-name (or venue "~/Code")))
           (left-win  (selected-window))
           (right-win (split-window left-win nil 'right)))
      ;; Left: eshell at venue cwd
      (with-selected-window left-win
        (let ((default-directory root)
              (display-buffer-overriding-action '(display-buffer-same-window)))
          (eshell t)))
      ;; Right: magit-status, falling back to dired
      (if (fboundp 'magit-status)
          (let ((default-directory root))
            (save-window-excursion (magit-status))
            (let ((magit-buf (cl-find-if
                              (lambda (b)
                                (with-current-buffer b
                                  (derived-mode-p 'magit-status-mode)))
                              (buffer-list))))
              (when magit-buf
                (set-window-buffer right-win magit-buf))))
        (set-window-buffer right-win (dired-noselect root)))
      (set-window-dedicated-p left-win  'bebop)
      (set-window-dedicated-p right-win 'bebop))))

(defun bebop-frame--close-solo (name)
  "Close the Solo frame for session NAME, if one exists."
  (let ((frame (cl-find-if
                 (lambda (f)
                   (equal (frame-parameter f 'bebop-solo-session) name))
                 (frame-list))))
    (when frame
      (delete-frame frame))))

(defun end-solo ()
  "Close a Bebop Solo frame and kill buffers shown only in it.
If the current frame is a Solo frame, ends that one directly.
Otherwise prompts to select which Solo to end.
Returns focus to the Conductor frame if one exists."
  (interactive)
  (let* ((solo-frame
          (if (frame-parameter (selected-frame) 'bebop-solo-session)
              (selected-frame)
            (let ((solos (cl-remove-if-not
                          (lambda (f) (frame-parameter f 'bebop-solo-session))
                          (frame-list))))
              (unless solos
                (user-error "No Bebop Solo frames open"))
              (if (= (length solos) 1)
                  (car solos)
                (let* ((names (mapcar (lambda (f)
                                        (frame-parameter f 'bebop-solo-session))
                                      solos))
                       (choice (completing-read "End solo: " names nil t)))
                  (cl-find-if (lambda (f)
                                (equal (frame-parameter f 'bebop-solo-session) choice))
                              solos))))))
         (conductor (cl-find-if (lambda (f) (frame-parameter f 'bebop-conductor))
                                (frame-list))))
    ;; Kill buffers that only appear in this solo frame
    (dolist (buf (delete-dups (mapcar #'window-buffer (window-list solo-frame))))
      (when (and (buffer-live-p buf)
                 (cl-every (lambda (w) (eq (window-frame w) solo-frame))
                            (get-buffer-window-list buf nil t)))
        ;; Suppress process-running query; keep modified-buffer query
        (let ((kill-buffer-query-functions
               (remq 'process-kill-buffer-query-function
                     kill-buffer-query-functions)))
          (kill-buffer buf))))
    (if conductor
        (progn
          (select-frame-set-input-focus conductor)
          (delete-frame solo-frame))
      (delete-frame solo-frame))))

(provide 'bebop-frame)

;;; bebop-frame.el ends here
