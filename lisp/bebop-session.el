;;; bebop-session.el --- Session lifecycle for Bebop agent orchestration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'claude-chant)
(require 'chant-dashboard)
(require 'chant-abeyance)

(defgroup bebop nil
  "Session-based AI agent orchestration built on Chant."
  :group 'tools)

(defcustom bebop-charts-dir "~/Code/Org/charts"
  "Directory for Bebop session chart files."
  :type 'string
  :group 'bebop)

(defcustom bebop-venues-dir "~/Code/Repos/Venues"
  "Directory for git worktrees (venues) created by Bebop sessions."
  :type 'string
  :group 'bebop)

(defcustom bebop-repos-dir "~/Code/Repos"
  "Base directory for git repositories available for venue creation."
  :type 'string
  :group 'bebop)

(defcustom bebop-header-color "#CC0000"
  "Color for Bebop header overlays in dashboard and composition buffers."
  :type 'string
  :group 'bebop)

(defcustom bebop-abeyance-header-color "#D4A017"
  "Header color for composition buffers when abeyance mode is active."
  :type 'string
  :group 'bebop)

(defvar bebop--sessions nil
  "Alist of (NAME . SINFO) for all known Bebop sessions.
SINFO is a plist with keys:
  :chart     — path to the session's chart .org file (string or nil)
  :venue     — path to the git worktree (string or nil)
  :gathering — non-nil while in gathering mode (pre-first-jam)")
(setq bebop--sessions nil)

(defvar-local bebop--header-overlay nil
  "Overlay used to display the Bebop header in a buffer.")

(defun bebop--set-header-overlay (text &optional color height)
  "Display TEXT as a styled header at point-min of the current buffer.
COLOR defaults to `bebop-header-color'; HEIGHT defaults to 3.0.
Uses the same overlay/before-string technique as `claude-chant--set-header'."
  (when (overlayp bebop--header-overlay)
    (delete-overlay bebop--header-overlay)
    (setq bebop--header-overlay nil))
  (let* ((resolved (when (fboundp 'claude-chant--resolve-font)
                     (claude-chant--resolve-font)))
         (c (or color bebop-header-color))
         (h (or height 3.0))
         (header (propertize text
                             'face `(:height ,h :weight bold
                                     :foreground ,c
                                     ,@(when resolved (list :family resolved)))
                             'read-only t
                             'front-sticky t
                             'rear-nonsticky t))
         (spacer (propertize "\n" 'read-only t))
         (ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'before-string (concat header spacer))
    (setq bebop--header-overlay ov)))

(defun bebop--toggle-abeyance ()
  "Toggle abeyance (keystroke passthrough) in this bebop composition buffer.
The header color changes to amber while active and returns to red when off."
  (interactive)
  (chant-abeyance-mode (if chant-abeyance-mode -1 1))
  (let ((name (and (string-match "\\*bebop-session: \\(.*\\)\\*" (buffer-name))
                   (match-string 1 (buffer-name)))))
    (bebop--set-header-overlay (format "Session: %s" name)
                               (if chant-abeyance-mode
                                   bebop-abeyance-header-color
                                 bebop-header-color))))

(defun bebop--dashboard-mode-after (&rest _)
  "Add the `Bebop' header overlay after `chant-dashboard-mode' activates.
Uses the same blue and height as the chant header."
  (bebop--set-header-overlay "Bebop" claude-chant-header-color claude-chant-header-height))

(advice-add 'chant-dashboard-mode :after #'bebop--dashboard-mode-after)

(defun bebop--session-info (name)
  "Return the session plist for NAME, or nil if not found."
  (cdr (assoc name bebop--sessions)))

(defun bebop--pair-gathering-p (name)
  "Return non-nil if session NAME is currently in gathering mode.
Called by `chant-dashboard--infer-status' to show the yellow dot."
  (let ((info (bebop--session-info name)))
    (and info (plist-get info :gathering))))

(defun bebop--session-chart (name)
  "Return the chart file path for session NAME, or nil."
  (let ((info (bebop--session-info name)))
    (and info (plist-get info :chart))))

(defun bebop--session-venue (name)
  "Return the venue (worktree) path for session NAME, or nil."
  (let ((info (bebop--session-info name)))
    (and info (plist-get info :venue))))

(defun bebop--ensure-dir (dir)
  "Expand DIR and create it (with parents) if it does not exist. Return the expanded path."
  (let ((expanded (expand-file-name dir)))
    (unless (file-directory-p expanded)
      (make-directory expanded t))
    expanded))

(defun bebop--list-repos ()
  "Return a list of repository directory names under `bebop-repos-dir'."
  (let ((dir (expand-file-name bebop-repos-dir)))
    (when (file-directory-p dir)
      (seq-filter (lambda (f)
                    (and (file-directory-p (expand-file-name f dir))
                         (not (string-prefix-p "." f))
                         (not (equal f "Venues"))))
                  (directory-files dir nil nil t)))))

(defun bebop--list-venues ()
  "Return a list of worktree directory names under `bebop-venues-dir'."
  (let ((dir (expand-file-name bebop-venues-dir)))
    (when (file-directory-p dir)
      (seq-filter (lambda (f)
                    (and (file-directory-p (expand-file-name f dir))
                         (not (string-prefix-p "." f))))
                  (directory-files dir nil nil t)))))

(defun bebop--list-branches (repo-path)
  "Return a list of branch names for the git repo at REPO-PATH."
  (let ((output (with-temp-buffer
                  (when (eq 0 (call-process "git" nil t nil
                                            "-C" repo-path
                                            "branch" "-a" "--format=%(refname:short)"))
                    (string-trim (buffer-string))))))
    (when output
      (seq-filter (lambda (b) (not (string-empty-p b)))
                  (split-string output "\n" t)))))

(defun bebop--sanitize-name (s)
  "Replace characters invalid in tmux window names or directory names with hyphens."
  (replace-regexp-in-string "[/: ]" "-" s))

(defun bebop--list-charts ()
  "Return a list of chart .org file names under `bebop-charts-dir'."
  (let ((dir (expand-file-name bebop-charts-dir)))
    (when (file-directory-p dir)
      (directory-files dir nil "\\.org\\'" t))))

(defun bebop--prompt-new-venue ()
  "Interactively pick a repo and branch, create a git worktree, and return its path."
  (let* ((repos (bebop--list-repos)))
    (unless repos
      (user-error "No repositories found in %s" bebop-repos-dir))
    (let* ((repo (completing-read "Repository: " repos nil t))
           (repo-path (expand-file-name repo (expand-file-name bebop-repos-dir)))
           (branches (bebop--list-branches repo-path))
           (branch (completing-read "Branch (type a new name to create): " branches))
           (_ (when (string-empty-p (string-trim branch))
                (user-error "Branch name cannot be empty")))
           (sanitized (bebop--sanitize-name branch))
           (worktree-name (format "%s--%s" repo sanitized))
           (venues-dir (bebop--ensure-dir bebop-venues-dir))
           (worktree-path (expand-file-name worktree-name venues-dir)))
      (when (file-directory-p worktree-path)
        (user-error "Worktree already exists: %s" worktree-path))
      (if (member branch branches)
          ;; Existing branch: add worktree without -b
          (unless (eq 0 (call-process "git" nil nil nil
                                      "-C" repo-path
                                      "worktree" "add"
                                      worktree-path branch))
            (user-error "git worktree add failed for existing branch: %s" branch))
        ;; New branch: create from HEAD with -b
        (unless (eq 0 (call-process "git" nil nil nil
                                    "-C" repo-path
                                    "worktree" "add" "-b" branch
                                    worktree-path "HEAD"))
          (user-error "git worktree add -b failed for new branch: %s" branch)))
      (message "Created worktree: %s" worktree-path)
      worktree-path)))

(defun bebop--prompt-existing-venue ()
  "Prompt for an existing venue worktree. Return its full path."
  (let ((venues (bebop--list-venues)))
    (unless venues
      (user-error "No venues found in %s" bebop-venues-dir))
    (let ((choice (completing-read "Existing venue: " venues nil t)))
      (expand-file-name choice (expand-file-name bebop-venues-dir)))))

(defun bebop--find-repo-for-venue (venue-path)
  "Guess the repository path for VENUE-PATH using the REPO--BRANCH naming convention."
  (let* ((venue-name (file-name-nondirectory (directory-file-name venue-path)))
         (parts (split-string venue-name "--" nil))
         (repo-name (car parts))
         (repo-path (expand-file-name repo-name (expand-file-name bebop-repos-dir))))
    (when (file-directory-p repo-path)
      repo-path)))

(defun bebop--create-new-chart (name)
  "Create a blank chart file for session NAME. Return the file path."
  (let* ((charts-dir (bebop--ensure-dir bebop-charts-dir))
         (path (expand-file-name (format "%s.org" name) charts-dir)))
    (unless (file-exists-p path)
      (with-temp-file path
        (insert (format "#+TITLE: %s\n" name))))
    path))

(defun bebop--prompt-existing-chart ()
  "Prompt for an existing chart file. Return its full path."
  (let* ((charts-dir (bebop--ensure-dir bebop-charts-dir))
         (files (bebop--list-charts)))
    (unless files
      (user-error "No chart files found in %s" bebop-charts-dir))
    (let ((choice (completing-read "Chart: " files nil t)))
      (expand-file-name choice charts-dir))))

(defun bebop--chart-section-for (name)
  "Return the chart section heading for session NAME based on its current state.
Returns \"Overture\" (gathering), \"Changes\" (active/blocked), or \"Coda\" (gone)."
  (if (bebop--pair-gathering-p name)
      "Overture"
    (let ((pair (cdr (assoc name chant-dashboard--pairs))))
      (if (and pair (eq (plist-get pair :status) 'gone))
          "Coda"
        "Changes"))))

(defun bebop--cue-to-chart (subtree chart-path section)
  "Append SUBTREE text to SECTION in chart at CHART-PATH.
Creates the section heading if it does not already exist."
  (with-current-buffer (find-file-noselect chart-path)
    (save-excursion
      ;; Strip the section heading from the start of SUBTREE if present.
      ;; This prevents a duplicate heading when the user cues from the
      ;; section heading itself (e.g. point is on "* Changes").
      (let* ((heading-re (format "\\`\\* %s[^\n]*\n?" (regexp-quote section)))
             (content (if (string-match heading-re subtree)
                          (substring subtree (match-end 0))
                        subtree)))
        (goto-char (point-min))
        (if (re-search-forward (format "^\\* %s\\b" (regexp-quote section)) nil t)
            ;; Section found: move to end of its content
            (org-end-of-subtree t)
          ;; Section absent: create it at the end of the file
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* %s\n" section)))
        ;; Append the content with a blank-line separator
        (unless (string-empty-p (string-trim content))
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (insert content)
          (unless (string-suffix-p "\n" content)
            (insert "\n")))))
    (save-buffer))
  (message "Cued to %s (%s)" section (file-name-nondirectory chart-path)))

(defun bebop--archive-chart (name chart-path)
  "Move the chart for session NAME from CHART-PATH to the archive directory."
  (let* ((archive-dir (bebop--ensure-dir
                        (expand-file-name "archive"
                                          (expand-file-name bebop-charts-dir))))
         (date-str (format-time-string "%m%d"))
         (archive-name (format "%s-%s.org" name date-str))
         (archive-path (expand-file-name archive-name archive-dir)))
    (rename-file chart-path archive-path t)
    (let ((buf (find-buffer-visiting chart-path)))
      (when buf (kill-buffer buf)))
    (message "Archived chart to %s" archive-path)))

(defun bebop--set-gathering (name value)
  "Set gathering state for session NAME to VALUE (non-nil = gathering, nil = active).
When VALUE is non-nil, also marks the dashboard entry as gathering immediately.
When VALUE is nil, lets the next poll cycle update the status from pane content."
  (let ((info (cdr (assoc name bebop--sessions))))
    (when info
      (plist-put info :gathering value)
      (when value
        (let ((pair (cdr (assoc name chant-dashboard--pairs))))
          (when pair
            (plist-put pair :status 'gathering))))
      (chant-dashboard--render))))

(defun bebop--make-send-fn (name)
  "Return a send function that clears gathering mode for NAME after sending."
  (lambda ()
    (interactive)
    (claude-chant-send-buffer)
    (bebop--set-gathering name nil)))

(defun bebop--get-or-create-composition-buffer (name)
  "Return the composition buffer for session NAME, creating it if absent."
  (let ((buf (get-buffer-create (format "*bebop-session: %s*" name))))
    (with-current-buffer buf
      (unless (eq major-mode 'text-mode)
        (text-mode))
      (claude-chant--apply-buffer-settings)
      (local-set-key (kbd "C-c C-c") (bebop--make-send-fn name))
      (local-set-key (kbd "M-a") #'bebop--toggle-abeyance)
      (bebop--set-header-overlay (format "Session: %s" name)))
    buf))

(defun bebop--create-session (name venue-path chart-path)
  "Create Bebop session NAME with optional VENUE-PATH and CHART-PATH.
Registers in `chant-dashboard--pairs' for dashboard display and polling,
registers in `bebop--sessions' for chart/venue/gathering tracking,
creates the per-session composition buffer, and starts in gathering mode."
  (when (assoc name chant-dashboard--pairs)
    (user-error "A pair named \"%s\" already exists in the dashboard" name))
  ;; Ensure the tmux session exists
  (unless (chant-dashboard--session-exists-p)
    (chant-dashboard--tmux "new-session" "-d" "-s" chant-dashboard-session))
  ;; Reject duplicate window names
  (when (member name (chant-dashboard--window-list))
    (user-error "tmux window \"%s\" already exists in session \"%s\""
                name chant-dashboard-session))
  (let* ((work-dir (expand-file-name (or venue-path "~/Code")))
         (target   (format "%s:%s" chant-dashboard-session name)))
    ;; Create the tmux window and launch Claude Code
    (chant-dashboard--tmux "new-window" "-t" chant-dashboard-session "-n" name "-a")
    (chant-dashboard--tmux "send-keys" "-t" target
                           (format "cd %s && claude"
                                   (shell-quote-argument work-dir))
                           "Enter")
    ;; Register with chant-dashboard (polling, status display, session switching)
    (push (cons name (list :window  target
                           :status  'gathering
                           :pane-id (chant-dashboard--pane-id-for name)))
          chant-dashboard--pairs)
    ;; Register with bebop (chart, venue, gathering state)
    (push (cons name (list :chart     chart-path
                           :venue     venue-path
                           :gathering t))
          bebop--sessions)
    ;; Auto-select if this is the first session
    (when (null chant-dashboard--active-pair)
      (chant-dashboard-select-pair name))
    ;; Set up the composition buffer
    (bebop--get-or-create-composition-buffer name)
    ;; Open chart in background (no buffer switch)
    (when (and chart-path (file-exists-p chart-path))
      (find-file-noselect chart-path))
    (chant-dashboard--render)
    (message "Bebop session \"%s\" created — gathering mode. Cue context with C-c C-p, then jam with C-c C-j." name)))

(defun bebop-new-session ()
  "Create a new Bebop session via the three-step flow: venue → name → chart.

Step 1 — Venue:
  New      → pick a repo from ~/Code/Repos/, pick/type a branch, create worktree
  Existing → pick an existing worktree from ~/Code/Repos/Venues/
  None     → no venue; session starts in ~/Code

Step 2 — Name: pre-filled from the venue's REPO--BRANCH directory name if a
  venue was chosen; otherwise blank and required.

Step 3 — Chart:
  New      → blank file created at ~/Code/Org/charts/NAME.org
  Existing → pick from ~/Code/Org/charts/"
  (interactive)
  ;; Step 1: Venue
  (let* ((venue-choice (completing-read "Venue: " '("New" "Existing" "None") nil t))
         (venue-path
          (cond
           ((equal venue-choice "New")      (bebop--prompt-new-venue))
           ((equal venue-choice "Existing") (bebop--prompt-existing-venue))
           (t nil))))
    ;; Step 2: Name
    (let* ((suggested (and venue-path
                           (file-name-nondirectory
                            (directory-file-name venue-path))))
           (name (read-string "Session name: " suggested)))
      (when (string-empty-p (string-trim name))
        (user-error "Session name cannot be empty"))
      (when (assoc name bebop--sessions)
        (user-error "Session \"%s\" already exists" name))
      ;; Step 3: Chart
      (let* ((chart-choice (completing-read "Chart: " '("New" "Existing") nil t))
             (chart-path
              (if (equal chart-choice "New")
                  (bebop--create-new-chart name)
                (bebop--prompt-existing-chart))))
        (bebop--create-session name venue-path chart-path)))))

(defun bebop-rename-session (old-name new-name)
  "Rename session OLD-NAME to NEW-NAME.
Updates the tmux window, both alist keys, the composition buffer name and
header, the chart file (when its basename matches OLD-NAME.org), and any
open Solo frame."
  (interactive
   (let* ((old (completing-read "Rename session: "
                                (mapcar #'car bebop--sessions)
                                nil t nil nil
                                chant-dashboard--active-pair))
          (new (read-string (format "New name for \"%s\": " old))))
     (list old new)))
  (when (string-empty-p (string-trim new-name))
    (user-error "Session name cannot be empty"))
  (when (equal old-name new-name)
    (user-error "New name is the same as the old name"))
  (when (assoc new-name chant-dashboard--pairs)
    (user-error "A session named \"%s\" already exists" new-name))
  (let* ((old-window  (format "%s:%s" chant-dashboard-session old-name))
         (new-window  (format "%s:%s" chant-dashboard-session new-name))
         (sinfo       (cdr (assoc old-name bebop--sessions)))
         (chart-path  (and sinfo (plist-get sinfo :chart))))
    ;; 1. Rename the tmux window
    (chant-dashboard--tmux "rename-window" "-t" old-window new-name)
    ;; 2. Update chant-dashboard--pairs key and :window target
    (let ((pair (assoc old-name chant-dashboard--pairs)))
      (when pair
        (setcar pair new-name)
        (plist-put (cdr pair) :window new-window)))
    ;; 3. Update bebop--sessions key
    (let ((entry (assoc old-name bebop--sessions)))
      (when entry
        (setcar entry new-name)))
    ;; 4. Update active-pair and chant target if this was the active session
    (when (equal chant-dashboard--active-pair old-name)
      (setq chant-dashboard--active-pair new-name)
      (setq claude-chant-target new-window))
    ;; 5. Rename composition buffer and refresh its header
    (let ((buf (get-buffer (format "*bebop-session: %s*" old-name))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (rename-buffer (format "*bebop-session: %s*" new-name))
          (bebop--set-header-overlay (format "Session: %s" new-name)
                                     (if chant-abeyance-mode
                                         bebop-abeyance-header-color
                                       bebop-header-color)))))
    ;; 6. Rename chart file when its basename matches OLD-NAME.org
    (when (and chart-path
               (string-equal (file-name-nondirectory chart-path)
                             (format "%s.org" old-name)))
      (let* ((dir           (file-name-directory chart-path))
             (new-chart     (expand-file-name (format "%s.org" new-name) dir)))
        (when (file-exists-p chart-path)
          (rename-file chart-path new-chart t))
        (let ((buf (find-buffer-visiting chart-path)))
          (when buf
            (with-current-buffer buf
              (set-visited-file-name new-chart t t))))
        (plist-put sinfo :chart new-chart)
        ;; Refresh conductor chart window if open
        (when (fboundp 'bebop-frame--update-conductor)
          (bebop-frame--update-conductor))))
    ;; 7. Update any open Solo frame
    (let ((solo (cl-find-if (lambda (f)
                              (equal (frame-parameter f 'bebop-solo-session) old-name))
                            (frame-list))))
      (when solo
        (set-frame-parameter solo 'bebop-solo-session new-name)
        (set-frame-parameter solo 'name (format "Solo: %s" new-name))))
    (chant-dashboard--render)
    (message "Renamed \"%s\" → \"%s\"" old-name new-name)))

(defun bebop-kill-session (name)
  "Kill session NAME and clean up all associated state.

Steps:
  1. Kill the tmux window (claude:NAME)
  2. Kill the *bebop-session: NAME* composition buffer
  3. Close the Solo frame for this session (if open)
  4. Archive the chart to ~/Code/Org/charts/archive/NAME-MMDD.org
  5. Offer to remove the venue worktree (git worktree remove)"
  (interactive
   (list (or (and bebop--sessions
                  (completing-read "Kill session: "
                                   (mapcar #'car bebop--sessions)
                                   nil t))
             (user-error "No Bebop sessions to kill"))))
  (unless (assoc name bebop--sessions)
    (user-error "Unknown Bebop session: %s" name))
  (let* ((sinfo     (bebop--session-info name))
         (chart-path (plist-get sinfo :chart))
         (venue-path (plist-get sinfo :venue))
         (pair-info  (cdr (assoc name chant-dashboard--pairs))))
    ;; 1. Kill tmux window
    (when pair-info
      (ignore-errors
        (chant-dashboard--tmux "kill-window" "-t" (plist-get pair-info :window))))
    ;; 2. Kill composition buffer
    (let ((buf (get-buffer (format "*bebop-session: %s*" name))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; 3. Close Solo frame if open
    (when (fboundp 'bebop-frame--close-solo)
      (bebop-frame--close-solo name))
    ;; 4. Archive chart
    (when (and chart-path (file-exists-p chart-path))
      (bebop--archive-chart name chart-path))
    ;; 5. Offer to remove venue worktree
    (when (and venue-path (file-directory-p venue-path))
      (when (yes-or-no-p (format "Remove venue worktree %s? " venue-path))
        (let ((repo-path (bebop--find-repo-for-venue venue-path)))
          (if repo-path
              (call-process "git" nil nil nil
                            "-C" repo-path "worktree" "remove" venue-path)
            (message "Could not determine repo for venue; skipping worktree removal")))))
    ;; Remove from registries
    (setq bebop--sessions
          (cl-remove-if (lambda (s) (equal (car s) name)) bebop--sessions))
    (setq chant-dashboard--pairs
          (cl-remove-if (lambda (p) (equal (car p) name)) chant-dashboard--pairs))
    ;; Update active pair if needed
    (when (equal chant-dashboard--active-pair name)
      (setq chant-dashboard--active-pair (caar chant-dashboard--pairs))
      (chant-dashboard--apply-active-pair))
    (chant-dashboard--render)
    (message "Killed Bebop session: %s" name)))

(defun bebop-reconnect-chart (session chart-file)
  "Re-link SESSION to CHART-FILE after a restart or other loss of session state.

SESSION must be a known tmux window (present in `chant-dashboard--pairs').
CHART-FILE is picked from `bebop-charts-dir'.

If the session has no entry in `bebop--sessions' (common after restart), a
minimal entry is created with the chart path and no venue. If an entry already
exists, only the chart path is updated.

After reconnecting, the Conductor frame's chart window is refreshed immediately."
  (interactive
   (list
    (completing-read "Session: "
                     (or (mapcar #'car chant-dashboard--pairs)
                         (user-error "No sessions known — open the dashboard first"))
                     nil t nil nil chant-dashboard--active-pair)
    (let* ((charts-dir (bebop--ensure-dir bebop-charts-dir))
           (files (or (bebop--list-charts)
                      (user-error "No chart files found in %s" bebop-charts-dir))))
      (expand-file-name (completing-read "Chart: " files nil t) charts-dir))))
  (if (assoc session bebop--sessions)
      ;; Session already registered — update chart path only
      (plist-put (cdr (assoc session bebop--sessions)) :chart chart-file)
    ;; Session missing from registry (post-restart) — create minimal entry
    (push (cons session (list :chart chart-file :venue nil :gathering nil))
          bebop--sessions))
  ;; Open the chart file in the background so it is ready to display
  (find-file-noselect chart-file)
  ;; Refresh the Conductor frame if open
  (when (fboundp 'bebop-frame--update-conductor)
    (bebop-frame--update-conductor))
  (message "Reconnected: %s → %s" session (file-name-nondirectory chart-file)))

(provide 'bebop-session)

;;; bebop-session.el ends here
