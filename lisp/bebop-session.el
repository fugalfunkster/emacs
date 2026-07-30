;;; bebop-session.el --- Session lifecycle for Bebop agent orchestration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'bebop-core)
(require 'bebop-dashboard)

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

(defcustom bebop-org-dir (expand-file-name "~/Code/org/")
  "Root directory searched recursively for .org files when picking a chart
via the \"org\" option."
  :type 'directory
  :group 'bebop)

(defcustom bebop-docker-script "~/Code/repos/my-claude/start.sh"
  "Path to the Docker Claude launcher script (my-claude/start.sh).
When `bebop-use-docker' is non-nil, new sessions run this instead of `claude'."
  :type 'string
  :group 'bebop)

(defvar bebop-use-docker nil
  "When non-nil, new sessions and resumes launch Claude inside the Docker sandbox.
Toggle with `bebop-toggle-docker' (bound to ! in the dashboard).")

(defun bebop-toggle-docker ()
  "Toggle Docker sandbox mode for new Bebop sessions.
When active, `bebop-new-session' and `bebop-resume' launch Claude inside
the container defined by `bebop-docker-script' instead of running it directly.
The git airgap (deny git push) is enforced by the container's settings."
  (interactive)
  (setq bebop-use-docker (not bebop-use-docker))
  (bebop--render)
  (message "Bebop: Docker sandbox %s" (if bebop-use-docker "ON" "OFF")))

(defvar bebop--sessions nil
  "Alist of (NAME . SINFO) for all known Bebop sessions.
SINFO is a plist with keys:
  :chart      — path to the session's chart .org file (string or nil)
  :venue      — path to the git worktree (string or nil)
  :created-at — timestamp string
  :last-active — timestamp string")

(defun bebop--on-deck-names ()
  "Return list of session names derivable from charts and venues on disk,
minus any currently running session names."
  (let* ((charts-dir (expand-file-name bebop-charts-dir))
         (venues-dir (expand-file-name bebop-venues-dir))
         (chart-names
          (when (file-directory-p charts-dir)
            (mapcar (lambda (f) (file-name-sans-extension f))
                    (directory-files charts-dir nil "\\.org$" t))))
         (venue-names
          (when (file-directory-p venues-dir)
            (seq-filter
             (lambda (f) (not (string-prefix-p "." f)))
             (seq-filter
              (lambda (f) (file-directory-p (expand-file-name f venues-dir)))
              (directory-files venues-dir nil nil t)))))
         (all-names (seq-uniq (append chart-names venue-names)))
         (running   (mapcar #'car bebop--live-sessions)))
    (seq-remove (lambda (n) (member n running)) all-names)))

(defun bebop--on-deck-icons (name)
  "Return icon string for NAME based on which artifacts exist on disk."
  (let ((has-chart (file-exists-p
                    (expand-file-name (concat name ".org")
                                      (expand-file-name bebop-charts-dir))))
        (has-venue (file-directory-p
                    (expand-file-name name
                                      (expand-file-name bebop-venues-dir)))))
    (concat (if has-chart "⊞" " ")
            (if has-venue " ⎇" ""))))

(defun bebop--on-deck-ticket-slug (name)
  "Return the ticket slug at the end of NAME (e.g. ROOST-1234), or nil."
  (when (string-match "\\([A-Z]+-[0-9]+\\)$" name)
    (match-string 1 name)))

(defun bebop--on-deck-prefix (name)
  "Return the component prefix of NAME: everything before the first --.
Returns an empty string for bare names like ROOST-1234."
  (let ((i (string-match "--" name)))
    (if i (substring name 0 i) "")))

(setq bebop-dashboard-footer-lines
      '("n: new  C/D: chart  V/W: venue  RET: select"
        "k: kill  e: exile  r: resume  g: refresh"))

(defun bebop-resume-at-point ()
  "Resume the on-deck session at point, or call `bebop-resume' interactively.
When point is on an on-deck line (has `bebop-on-deck-name' text property),
call `bebop-resume' with that name pre-filled.  Otherwise call `bebop-resume'
with no argument."
  (interactive)
  (let ((name (get-text-property (point) 'bebop-on-deck-name)))
    (if name
        (bebop-resume name)
      (bebop-resume))))

(defun bebop--dashboard-entry-at-point-p ()
  "Return non-nil if point is on a navigable dashboard entry line."
  (or (get-text-property (point) 'bebop-session-name)
      (get-text-property (point) 'bebop-on-deck-name)))

(defun bebop--dashboard-next-entry ()
  "Move point to the next active or on-deck session line."
  (interactive)
  (let ((start (point)))
    (when (bebop--dashboard-entry-at-point-p)
      (forward-line 1))
    (while (and (< (point) (point-max))
                (not (bebop--dashboard-entry-at-point-p)))
      (forward-line 1))
    (unless (bebop--dashboard-entry-at-point-p)
      (goto-char start))))

(defun bebop--dashboard-prev-entry ()
  "Move point to the previous active or on-deck session line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (> (point) (point-min))
                (not (bebop--dashboard-entry-at-point-p)))
      (forward-line -1))
    (unless (bebop--dashboard-entry-at-point-p)
      (goto-char start))))

(defun bebop-kill-session-at-point ()
  "Kill the Bebop session at point.  Requires typing the name to confirm."
  (interactive)
  (let ((name (or (get-text-property (point) 'bebop-session-name)
                  (bebop--session-at-point))))
    (if (null name)
        (message "No session at point")
      (let ((typed (read-string (format "Type \"%s\" (or \"see ya\") to confirm kill: " name))))
        (if (or (string= typed name) (string= typed "see ya"))
            (bebop-kill-session name)
          (message "Cancelled (name mismatch)."))))))

;; Override nav keys to include on-deck lines; k = kill, r = resume, a = archive-chart
(define-key bebop-dashboard-mode-map (kbd "<down>") #'bebop--dashboard-next-entry)
(define-key bebop-dashboard-mode-map (kbd "<up>")   #'bebop--dashboard-prev-entry)
(define-key bebop-dashboard-mode-map (kbd "j")      #'bebop--dashboard-next-entry)
(define-key bebop-dashboard-mode-map (kbd "p")      #'bebop--dashboard-prev-entry)
(define-key bebop-dashboard-mode-map (kbd "k") #'bebop-kill-session-at-point)
(define-key bebop-dashboard-mode-map (kbd "r") #'bebop-resume-at-point)
(define-key bebop-dashboard-mode-map (kbd "e") #'bebop-exile-session-at-point)

(defun bebop--session-info (name)
  "Return the session plist for NAME, or nil if not found."
  (cdr (assoc name bebop--sessions)))

(defun bebop--now-string ()
  "Return current timestamp string in ISO-like format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun bebop--upsert-session (name props)
  "Create or update session NAME with PROPS plist."
  (let ((entry (assoc name bebop--sessions))
        (now (bebop--now-string)))
    (if entry
        (let ((info (cdr entry)))
          (while props
            (plist-put info (car props) (cadr props))
            (setq props (cddr props)))
          (unless (plist-get info :created-at)
            (plist-put info :created-at now))
          (unless (plist-get info :last-active)
            (plist-put info :last-active now)))
      (let ((base (list :chart nil
                        :venue nil
                        :created-at now
                        :last-active now)))
        (while props
          (plist-put base (car props) (cadr props))
          (setq props (cddr props)))
        (push (cons name base) bebop--sessions)))))

(defun bebop--mark-active-session (name)
  "Update last-active timestamp for session NAME."
  (when name
    (let ((entry (assoc name bebop--sessions)))
      (when entry
        (plist-put (cdr entry) :last-active (bebop--now-string))))))

(defun bebop-reconcile-sessions ()
  "Reconcile `bebop--sessions' against dashboard-discovered tmux pairs."
  (let ((now (bebop--now-string)))
    ;; 1. Mark all known sessions as lost (assume no live pair).
    (dolist (entry bebop--sessions)
      (let ((info (cdr entry)))
        (unless (plist-get info :created-at)
          (plist-put info :created-at now))
        (unless (plist-get info :last-active)
          (plist-put info :last-active now))))
    ;; 2. Ensure every live dashboard pair exists in the registry.
    ;; Resolve chart/venue from disk — both for new entries and existing ones
    ;; where the values are still nil (e.g. sessions discovered before this fix).
    (dolist (pair bebop--live-sessions)
      (let* ((name  (car pair))
             (entry (assoc name bebop--sessions))
             (info  (cdr entry))
             (chart-path (expand-file-name (concat name ".org")
                                           (expand-file-name bebop-charts-dir)))
             (venue-path (expand-file-name name
                                           (expand-file-name bebop-venues-dir)))
             (chart (and (file-exists-p chart-path) chart-path))
             (venue (and (file-directory-p venue-path) venue-path)))
        (if entry
            (progn
              (when (and chart (null (plist-get info :chart)))
                (plist-put info :chart chart))
              (when (and venue (null (plist-get info :venue)))
                (plist-put info :venue venue)))
          (push (cons name (list :chart chart
                                 :venue venue
                                 :created-at now
                                 :last-active now))
                bebop--sessions))))
    ;; 3. Remove entries with no live pair.
    (setq bebop--sessions
          (seq-filter
           (lambda (entry)
             (assoc (car entry) bebop--live-sessions))
           bebop--sessions))
    (setq bebop--sessions
          (sort bebop--sessions (lambda (a b) (string< (car a) (car b)))))))

(advice-add 'bebop--discover-existing-sessions :after
            (lambda (&rest _) (bebop-reconcile-sessions)))

(advice-add 'bebop--apply-active-session :after
            (lambda (&rest _) (bebop--mark-active-session bebop--active-session)))

(defun bebop--session-chart (name)
  "Return the chart file path for session NAME, or nil."
  (let ((info (bebop--session-info name)))
    (and info (plist-get info :chart))))

(defun bebop-jam (name text)
  "Send TEXT to session NAME's agent as a prompt.
Citizen verb: callable over the emacs server by skills, sibling
agents, and remote machines (e.g. the cross-machine handoff relay).
Uses the same load-buffer → paste-buffer → C-m pattern as
`bebop-send-buffer' so multi-line TEXT arrives as a single prompt.
Signals if NAME has no live tmux window."
  (let* ((window (bebop--tmux-window-name name))
         (pane (bebop--tmux-pane-id-for window)))
    (unless pane
      (user-error "bebop-jam: no live session named %s" name))
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max)
                           "tmux" nil nil nil "load-buffer" "-"))
    (call-process "tmux" nil nil nil "paste-buffer" "-d" "-t" pane)
    (call-process "tmux" nil nil nil "send-keys" "-t" pane "C-m")
    (run-hook-with-args 'bebop-session-activity-functions name 'send)
    (format "jammed: %s" name)))

(defun bebop--session-venue (name)
  "Return the venue (worktree) path for session NAME, or nil."
  (let ((info (bebop--session-info name)))
    (and info (plist-get info :venue))))

(defun bebop--session-names ()
  "Return list of known session names."
  (mapcar #'car bebop--sessions))

(defun bebop--assert-name-available (name)
  "Signal a user-error if NAME is empty or already used by a running session."
  (when (string-empty-p (string-trim name))
    (user-error "Session name cannot be empty"))
  (when (assoc name bebop--live-sessions)
    (user-error "Session \"%s\" is already running" name)))

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

(defun bebop--prompt-repo-dir ()
  "Prompt user to pick a plain repo directory from `bebop-repos-dir'.
Returns the absolute path of the chosen directory.
Unlike `bebop--prompt-new-venue', no worktree is created."
  (let* ((repos-dir (expand-file-name bebop-repos-dir))
         (entries
          (seq-filter
           (lambda (f)
             (and (file-directory-p (expand-file-name f repos-dir))
                  (not (equal f "Venues"))
                  (not (string-prefix-p "." f))))
           (directory-files repos-dir nil nil t)))
         (_ (unless entries
              (user-error "No repo directories found in %s" repos-dir)))
         (choice (completing-read "Repo: " entries nil t)))
    (expand-file-name choice repos-dir)))

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

(defun bebop--prompt-any-org ()
  "Prompt user to pick any .org file under `bebop-org-dir'.
Shows paths relative to bebop-org-dir for readability.
Returns the absolute path."
  (let* ((root  (expand-file-name bebop-org-dir))
         (files (directory-files-recursively root "\\.org$"))
         ;; Exclude archive subdir
         (files (seq-remove (lambda (f) (string-match-p "/archive/" f)) files))
         (display (mapcar (lambda (f) (file-relative-name f root)) files))
         (_ (unless display
              (user-error "No .org files found under %s" bebop-org-dir)))
         (choice (completing-read "Org doc: " display nil t)))
    (expand-file-name choice root)))

;; Only called from org-mode buffers, where org is already loaded —
;; no hard (require 'org) needed.
(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-subtree "org")

(defun bebop--subtree-text ()
  "Return the full org subtree at point as a string.
Signals a user error if point is not within a heading.
(Rehomed from the retired bebop-cue module — still used by the
\"from heading\" chart option in `bebop-new-session'.)"
  (save-excursion
    (condition-case _
        (progn
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          (let* ((beg (point))
                 (end (progn (org-end-of-subtree t t) (point))))
            (buffer-substring-no-properties beg end)))
      (error (user-error "Point is not within an org heading")))))

(defun bebop--cue-to-chart (subtree chart-path)
  "Append SUBTREE text to the end of the chart at CHART-PATH."
  (with-current-buffer (find-file-noselect chart-path)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n")
      (insert subtree)
      (unless (string-suffix-p "\n" subtree)
        (insert "\n")))
    (save-buffer))
  (message "Cued to %s" (file-name-nondirectory chart-path)))

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

(defun bebop-archive-chart (name)
  "Manually archive the chart for session NAME to the archive subdirectory.
The session is not killed; only the chart file is moved."
  (let ((chart-path (bebop--session-chart name)))
    (unless (and chart-path (file-exists-p chart-path))
      (user-error "No chart file found for session: %s" name))
    (bebop--archive-chart name chart-path)
    (when-let ((buf (find-buffer-visiting chart-path)))
      (kill-buffer buf))
    (bebop--upsert-session name (list :chart nil))
    (message "Archived chart for session: %s" name)))

(defun bebop--remove-venue (venue-path)
  "Remove the git worktree at VENUE-PATH.
Tries `git worktree remove --force' first to properly unregister the worktree
from the parent repo.  Falls back to `delete-directory' if git fails (e.g.
the parent repo is gone)."
  (let ((repo-path (bebop--find-repo-for-venue venue-path)))
    (if (and repo-path
             (eq 0 (call-process "git" nil nil nil
                                 "-C" repo-path
                                 "worktree" "remove" "--force" venue-path)))
        (message "Removed worktree: %s" venue-path)
      (delete-directory venue-path t)
      (message "Removed venue directory: %s" venue-path))))

(defun bebop-exile-session (name)
  "Exile on-deck session NAME: archive its chart and delete its venue.
The session will not appear in On deck and cannot be resumed.

Steps:
  1. Archive chart to the archive subdirectory (if present on disk)
  2. Delete venue via git worktree remove --force, or rm -rf (if present)
  3. Refresh the dashboard"
  (interactive
   (list (completing-read "Exile session: "
                          (bebop--on-deck-names) nil t)))
  (let* ((chart-path (let ((f (expand-file-name
                               (concat name ".org")
                               (expand-file-name bebop-charts-dir))))
                       (when (file-exists-p f) f)))
         (venue-path (let ((d (expand-file-name
                               name
                               (expand-file-name bebop-venues-dir))))
                       (when (file-directory-p d) d)))
         (desc (cond
                ((and chart-path venue-path) "archive chart + delete venue")
                (chart-path                  "archive chart (no venue)")
                (venue-path                  "delete venue (no chart)")
                (t                           "(no artifacts)"))))
    (unless (yes-or-no-p (format "Exile \"%s\"? (%s) " name desc))
      (user-error "Exile cancelled"))
    (when chart-path
      (bebop--archive-chart name chart-path))
    (when venue-path
      (bebop--remove-venue venue-path))
    (bebop-refresh)
    (message "Exiled session: %s" name)))

(defun bebop-exile-session-at-point ()
  "Exile the on-deck session at point, or call `bebop-exile-session' interactively.
When point has a `bebop-on-deck-name' text property, passes that name directly
to `bebop-exile-session'.  Otherwise falls through to the interactive prompt."
  (interactive)
  (let ((session-name (get-text-property (point) 'bebop-on-deck-name)))
    (if session-name
        (bebop-exile-session session-name)
      (bebop-exile-session))))

(defvar-local bebop-composition--cookies nil
  "Face-remapping cookies active while `bebop-composition-mode' is on.")

(defvar-local bebop--composition-session nil
  "Session name this composition buffer belongs to, or nil for the global buffer.")

(defvar bebop-composition-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'bebop-composition-send)
    map)
  "Keymap for `bebop-composition-mode'.")

(defun bebop-composition-send ()
  "Send this composition buffer to its Claude session."
  (interactive)
  (bebop-send-buffer)
  (when-let ((name (or bebop--composition-session bebop--active-session)))
    (run-hook-with-args 'bebop-session-activity-functions name 'send)))

(defun bebop-composition--enable ()
  "Activate composition-buffer settings in the current buffer."
  (setq-local org-hide-emphasis-markers t)
  (setq-local org-pretty-entities t)
  (setq-local org-use-sub-superscripts nil)
  (setq-local line-spacing 0.15)
  (visual-line-mode 1)
  (when (bound-and-true-p flyspell-mode) (flyspell-mode -1))
  (when (bound-and-true-p company-mode) (company-mode -1))
  (setq-local completion-at-point-functions
              (remove 'ispell-completion-at-point
                      completion-at-point-functions))
  ;; Hide standard mode-line; use header-line instead (mirrors dashboard)
  (setq-local mode-line-format nil)
  (setq-local header-line-format
              '((:eval
                 (let* ((resolved (and (fboundp 'bebop--resolve-font) (bebop--resolve-font)))
                        (base-h (let ((dh (face-attribute 'default :height nil t)))
                                  (if (integerp dh) dh 160)))
                        (abs-h (round (* bebop-header-height base-h)))
                        (label (if bebop--composition-session
                                   (format "Session: %s" bebop--composition-session)
                                 "Bebop")))
                   (propertize (concat " " label)
                               'face `(:height ,abs-h :weight bold
                                       :foreground ,bebop-session-header-color
                                       ,@(when resolved (list :family resolved))))))))
  (face-remap-add-relative 'header-line
                           :background (face-attribute 'default :background nil t)
                           :box nil
                           :underline nil)
  (setq bebop-composition--cookies
        (list
         (face-remap-add-relative 'default          :family "CamingoCode")
         (face-remap-add-relative 'variable-pitch   :family "CamingoCode")
         (face-remap-add-relative 'fixed-pitch       :family "CamingoCode")
         (face-remap-add-relative 'org-block              :inherit 'fixed-pitch)
         (face-remap-add-relative 'org-block-begin-line   :inherit 'fixed-pitch)
         (face-remap-add-relative 'org-block-end-line     :inherit 'fixed-pitch)
         (face-remap-add-relative 'org-code               :inherit 'fixed-pitch)
         (face-remap-add-relative 'org-verbatim           :inherit 'fixed-pitch)
         (face-remap-add-relative 'org-level-1      :family "CamingoCode" :height 1.0 :weight 'bold)
         (face-remap-add-relative 'org-level-2      :family "CamingoCode" :height 1.0 :weight 'bold)
         (face-remap-add-relative 'org-level-3      :family "CamingoCode" :height 1.0 :weight 'bold)
         (face-remap-add-relative 'org-level-4      :family "CamingoCode" :height 1.0 :weight 'bold))))

(defun bebop-composition--disable ()
  "Deactivate composition-buffer settings in the current buffer."
  (dolist (cookie bebop-composition--cookies)
    (face-remap-remove-relative cookie))
  (setq bebop-composition--cookies nil)
  (kill-local-variable 'org-hide-emphasis-markers)
  (kill-local-variable 'org-pretty-entities)
  (kill-local-variable 'org-use-sub-superscripts)
  (kill-local-variable 'line-spacing)
  (kill-local-variable 'mode-line-format)
  (kill-local-variable 'header-line-format)
  (visual-line-mode -1)
  (company-mode 1))

(define-minor-mode bebop-composition-mode
  "Book-like prose mode for Bebop composition buffers.

Layers variable-pitch fonts, soft word-wrap, and per-face font overrides
onto org-mode without altering any global org settings.

\\{bebop-composition-mode-map}"
  :lighter " ♩"
  (if bebop-composition-mode
      (bebop-composition--enable)
    (bebop-composition--disable)))

(defun bebop--get-or-create-composition-buffer (name)
  "Return the composition buffer for session NAME, creating it if absent."
  (let ((buf (get-buffer-create (format "*bebop-session: %s*" name))))
    (with-current-buffer buf
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (unless bebop-composition-mode
        (bebop-composition-mode 1))
      (setq-local bebop--composition-session name)
      (bebop--apply-buffer-settings)
      (local-set-key (kbd "M-a") #'bebop-passthrough))
    buf))

(defun bebop--create-session (name venue-path chart-path &optional launcher)
  "Create Bebop session NAME with optional VENUE-PATH and CHART-PATH.
LAUNCHER, if non-nil, overrides the default `claude' invocation.  Pass
\\='docker to run Claude inside the sandbox defined by `bebop-docker-script'.
Registers in `bebop--live-sessions' for dashboard display and polling,
registers in `bebop--sessions' for chart/venue/pending tracking,
creates the per-session composition buffer, and starts in pending mode."
  (when (assoc name bebop--live-sessions)
    (user-error "A pair named \"%s\" already exists in the dashboard" name))
  ;; Ensure the tmux session exists
  (unless (bebop--tmux-session-exists-p)
    (bebop--tmux "new-session" "-d" "-s" bebop-tmux-session))
  ;; Reject duplicate window names
  (when (member name (bebop--tmux-window-list))
    (user-error "tmux window \"%s\" already exists in session \"%s\""
                name bebop-tmux-session))
  (let* ((work-dir (expand-file-name (or venue-path "~/Code")))
         (target   (format "%s:%s" bebop-tmux-session name))
         ;; BEBOP_SESSION carries the session's identity into the agent's
         ;; environment — the hook/skill orientation path keys off it.
         (env      (format "BEBOP_SESSION=%s" (shell-quote-argument name)))
         (launch-cmd (if (eq launcher 'docker)
                         (format "cd %s && %s %s"
                                 (shell-quote-argument work-dir)
                                 env
                                 (expand-file-name bebop-docker-script))
                       (format "cd %s && %s claude"
                               (shell-quote-argument work-dir)
                               env))))
    ;; Create the tmux window and launch Claude Code
    (bebop--tmux "new-window" "-t" bebop-tmux-session "-n" name "-a")
    (bebop--tmux "send-keys" "-t" target launch-cmd "Enter")
    ;; Register with bebop-dashboard (polling, status display, session switching)
    (push (cons name (list :window  target
                           :status  'unknown
                           :pane-id (bebop--tmux-pane-id-for name)))
          bebop--live-sessions)
    ;; Register with bebop (chart, venue, launcher, last-active)
    (bebop--upsert-session name (list :chart chart-path
                                      :venue venue-path
                                      :launcher launcher
                                      :last-active (bebop--now-string)))
    ;; Select the new session in bebop
    (bebop-select-session name)
    ;; Set up the composition buffer
    (bebop--get-or-create-composition-buffer name)
    ;; Open chart in background (no buffer switch)
    (when (and chart-path (file-exists-p chart-path))
      (find-file-noselect chart-path))
    (bebop--render)
    (message "Bebop session \"%s\" created." name)))

(defun bebop-new-session ()
  "Create a new Bebop session.

Context-aware: detects whether point is in an org buffer or at a heading and
offers appropriate shortcuts in the chart prompt.

Flow:
  1. Venue  — new / old / repo / none
  2. Name   — editable string; pre-filled from venue dirname if a venue chosen
  3. Chart  — context-dependent options (see below)

Chart options vary by context:
  - In org-mode at a heading: \"from heading\" / new / old / org / none
  - In org-mode (any):        \"use this file\" / new / old / org / none
  - Elsewhere:                new / old / org / none

\"from heading\": creates a new chart in bebop-charts-dir and appends the
current subtree to its * Overture section.

\"use this file\": sets the chart to the current buffer's file path (any
.org file, not limited to bebop-charts-dir).

\"old\": pick from bebop-charts-dir.
\"org\": pick any .org under bebop-org-dir (see bebop--prompt-any-org).
\"new\": create a new blank chart at bebop-charts-dir/NAME.org."
  (interactive)
  (let* (;; ── Step 1: Venue ────────────────────────────────────────────────
         (venue-choice (completing-read "Venue: "
                                        '("new" "old" "repo" "none") nil t))
         (venue-path
          (cond
           ((equal venue-choice "new")  (bebop--prompt-new-venue))
           ((equal venue-choice "old")  (bebop--prompt-existing-venue))
           ((equal venue-choice "repo") (bebop--prompt-repo-dir))
           (t nil)))
         ;; ── Step 2: Name ─────────────────────────────────────────────────
         (default-name (when venue-path
                         (file-name-nondirectory
                          (directory-file-name venue-path))))
         (name (read-string "Session name: " default-name))
         (_ (bebop--assert-name-available name))
         ;; ── Step 3: Chart (context-aware) ────────────────────────────────
         (in-org    (derived-mode-p 'org-mode))
         (at-heading (and in-org
                          (ignore-errors (save-excursion (org-back-to-heading t) t))))
         (current-file (buffer-file-name))
         (chart-options
          (append
           (when at-heading  '("from heading"))
           (when (and in-org current-file) '("use this file"))
           '("new" "old" "org" "none")))
         (chart-choice (completing-read "Chart: " chart-options nil t))
         (chart-path
          (cond
           ((equal chart-choice "from heading")
            (let ((subtree (bebop--subtree-text))
                  (path    (bebop--create-new-chart name)))
              (bebop--cue-to-chart subtree path)
              path))
           ((equal chart-choice "use this file") current-file)
           ((equal chart-choice "new")  (bebop--create-new-chart name))
           ((equal chart-choice "old")  (bebop--prompt-existing-chart))
           ((equal chart-choice "org")  (bebop--prompt-any-org))
           (t nil))))
    (bebop--create-session name venue-path chart-path
                           (when bebop-use-docker 'docker))))

(defun bebop-kill-session (name)
  "Kill session NAME and clean up all associated state.

Steps:
  1. Kill the tmux window (claude:NAME)
  2. Kill the *bebop-session: NAME* composition buffer
  3. Close the Solo frame for this session (if open)
  4. Close the chart buffer (file remains in bebop-charts-dir)

Chart and venue artifacts are left on disk and will appear in On deck."
  (let* ((sinfo     (bebop--session-info name))
         (chart-path (plist-get sinfo :chart))
         (venue-path (plist-get sinfo :venue))
         (pair-info  (cdr (assoc name bebop--live-sessions))))
    ;; 1. Kill tmux window
    (when pair-info
      (ignore-errors
        (bebop--tmux "kill-window" "-t" (plist-get pair-info :window))))
    ;; 2. Kill composition buffer
    (let ((buf (get-buffer (format "*bebop-session: %s*" name))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; 3. Close Solo frame if open — offer buffer cleanup
    (let ((solo-frame (cl-find-if
                       (lambda (f) (equal (frame-parameter f 'bebop-solo-session) name))
                       (frame-list))))
      (when solo-frame
        (if (yes-or-no-p (format "Solo frame for \"%s\" is open. Close it and clean up buffers? " name))
            (progn
              (dolist (buf (delete-dups (mapcar #'window-buffer (window-list solo-frame))))
                (when (and (buffer-live-p buf)
                           (cl-every (lambda (w) (eq (window-frame w) solo-frame))
                                     (get-buffer-window-list buf nil t)))
                  (let ((kill-buffer-query-functions
                         (remq 'process-kill-buffer-query-function
                               kill-buffer-query-functions)))
                    (kill-buffer buf))))
              (delete-frame solo-frame))
          (message "Solo frame left open — it will be stale after this kill."))))
    ;; 4. Close chart buffer (file remains in bebop-charts-dir)
    (when chart-path
      (let ((buf (find-buffer-visiting chart-path)))
        (when buf (kill-buffer buf))))
    ;; Remove from registries (simple — no state transition)
    (setq bebop--sessions
          (cl-remove-if (lambda (s) (equal (car s) name)) bebop--sessions))
    (setq bebop--live-sessions
          (cl-remove-if (lambda (p) (equal (car p) name)) bebop--live-sessions))
    ;; Update active pair if needed
    (when (equal bebop--active-session name)
      (setq bebop--active-session (caar bebop--live-sessions))
      (bebop--apply-active-session))
    (bebop-refresh)
    (message "Killed Bebop session: %s" name)))

(defun bebop-resume (&optional name)
  "Resume a Bebop session using existing artifacts on disk.

Scans bebop-charts-dir and bebop-venues-dir for matching names and
pre-fills the session creation flow.  NAME may be passed from an
on-deck dashboard line."
  (let* ((names (bebop--on-deck-names))
         (_ (unless names
              (user-error "No sessions on deck (no charts or venues on disk)")))
         (chosen (or name
                     (completing-read "On deck: " names nil t)))
         (chart-path (let ((f (expand-file-name
                               (concat chosen ".org")
                               (expand-file-name bebop-charts-dir))))
                       (when (file-exists-p f) f)))
         (venue-path (let ((d (expand-file-name
                               chosen
                               (expand-file-name bebop-venues-dir))))
                       (when (file-directory-p d) d)))
         (session-name (read-string "Session name: " chosen))
         (_ (bebop--assert-name-available session-name))
         ;; Respect the Docker flag; fall back to the launcher the session was
         ;; originally created with (stored in :launcher) if the flag is off.
         (stored   (bebop--session-info chosen))
         (launcher (if bebop-use-docker 'docker
                     (and stored (plist-get stored :launcher)))))
    (bebop--create-session session-name venue-path chart-path launcher)))

(defun bebop-associate-chart (name)
  "Associate an org doc as the chart for session NAME.
Errors if session already has a chart — use `bebop-dissociate-chart' first."
  (interactive
   (list (completing-read "Session: "
                          (bebop--session-names) nil t)))
  (when (bebop--session-chart name)
    (user-error "Session \"%s\" already has a chart — use bebop-dissociate-chart first" name))
  (let* ((choice (completing-read "Chart: " '("new" "old" "org") nil t))
         (chart-path
          (cond
           ((equal choice "new") (bebop--create-new-chart name))
           ((equal choice "old") (bebop--prompt-existing-chart))
           (t                    (bebop--prompt-any-org)))))
    (bebop--upsert-session name (list :chart chart-path))
    (find-file-noselect chart-path)
    (bebop--render)
    (message "Associated chart: %s" (file-name-nondirectory chart-path))))

(defun bebop-dissociate-chart (name)
  "Remove the chart association from session NAME."
  (interactive
   (list (completing-read "Session: "
                          (seq-filter #'bebop--session-chart
                                      (bebop--session-names))
                          nil t)))
  (let ((chart (bebop--session-chart name)))
    (when-let ((buf (and chart (find-buffer-visiting chart))))
      (kill-buffer buf))
    (bebop--upsert-session name (list :chart nil))
    (bebop--render)
    (message "Dissociated chart from session: %s" name)))

(defun bebop-associate-venue (name)
  "Associate a repo or worktree as the venue for session NAME.
Errors if session already has a venue — use `bebop-dissociate-venue' first.
Note: does not move the running Claude process's working directory."
  (interactive
   (list (completing-read "Session: "
                          (bebop--session-names) nil t)))
  (when (bebop--session-venue name)
    (user-error "Session \"%s\" already has a venue — use bebop-dissociate-venue first" name))
  (let* ((choice (completing-read "Venue: " '("new" "old" "repo") nil t))
         (venue-path
          (cond
           ((equal choice "new")  (bebop--prompt-new-venue))
           ((equal choice "old")  (bebop--prompt-existing-venue))
           (t                     (bebop--prompt-repo-dir)))))
    (bebop--upsert-session name (list :venue venue-path))
    (bebop--render)
    (message "Associated venue: %s" (file-name-nondirectory
                                     (directory-file-name venue-path)))))

(defun bebop-dissociate-venue (name)
  "Remove the venue association from session NAME."
  (interactive
   (list (completing-read "Session: "
                          (seq-filter #'bebop--session-venue
                                      (bebop--session-names))
                          nil t)))
  (bebop--upsert-session name (list :venue nil))
  (bebop--render)
  (message "Dissociated venue from session: %s" name))

(defun bebop-associate-chart-at-point ()
  "Associate a chart for the session at point."
  (interactive)
  (let ((name (get-text-property (point) 'bebop-session-name)))
    (if name (bebop-associate-chart name)
      (call-interactively #'bebop-associate-chart))))

(defun bebop-dissociate-chart-at-point ()
  "Dissociate chart from the session at point."
  (interactive)
  (let ((name (get-text-property (point) 'bebop-session-name)))
    (if name (bebop-dissociate-chart name)
      (call-interactively #'bebop-dissociate-chart))))

(defun bebop-associate-venue-at-point ()
  "Associate a venue for the session at point."
  (interactive)
  (let ((name (get-text-property (point) 'bebop-session-name)))
    (if name (bebop-associate-venue name)
      (call-interactively #'bebop-associate-venue))))

(defun bebop-dissociate-venue-at-point ()
  "Dissociate venue from the session at point."
  (interactive)
  (let ((name (get-text-property (point) 'bebop-session-name)))
    (if name (bebop-dissociate-venue name)
      (call-interactively #'bebop-dissociate-venue))))

(define-key bebop-dashboard-mode-map (kbd "C") #'bebop-associate-chart-at-point)
(define-key bebop-dashboard-mode-map (kbd "D") #'bebop-dissociate-chart-at-point)
(define-key bebop-dashboard-mode-map (kbd "V") #'bebop-associate-venue-at-point)
(define-key bebop-dashboard-mode-map (kbd "W") #'bebop-dissociate-venue-at-point)
(define-key bebop-dashboard-mode-map (kbd "!") #'bebop-toggle-docker)

(provide 'bebop-session)

;;; bebop-session.el ends here
