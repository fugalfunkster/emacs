;;; bebop-dashboard.el --- Multi-agent dashboard for Claude tmux panes -*- lexical-binding: t; -*-

;; Usage:
;;   M-x bebop-dashboard-open   open the dashboard + composition buffer split layout
;;   bebop (in eshell)          open the Conductor frame
;;
;; The dashboard lists all active Claude agent pairs with status indicators.
;; Red dot = Claude needs your input to continue (permission prompt or question).
;; Green dot = Claude is active/responding or idle at the prompt.

(require 'bebop-core)
(require 'magit-section)

(defcustom bebop-poll-interval 4
  "Seconds between tmux pane status polls."
  :type 'integer
  :group 'bebop)

(defcustom bebop-tmux-session "claude"
  "Name of the tmux session used for all agent windows."
  :type 'string
  :group 'bebop)

(defcustom bebop-buffer-name "*bebop*"
  "Buffer name for the agent dashboard."
  :type 'string
  :group 'bebop)

(defcustom bebop-active-indicators '("Chanting to the machine spirit")
  "List of regexps/strings indicating Claude is actively generating output.
Checked after blocked-indicators. Matched against the last 8 non-empty pane
lines.

\"Chanting to the machine spirit\" is the configured custom spinner verb
(settings.json spinnerVerbs). Matches only the present-tense spinner text that
appears while Claude is working. Does not match the past-tense completion line
\"Chanted to the machine spirit for 3m 41s\".

Narrowed from the original four indicators because the ellipsis was causing
false positives — any \"...\" in pane output (paths, prose, etc.) would trigger
active.

NOTE: do not add \"accept edits\" here — that appears in the status bar whenever
auto-accept-edits mode is on, regardless of activity. It is a mode indicator,
not an activity signal."
  :type '(repeat string)
  :group 'bebop)

(defcustom bebop-blocked-indicators '("Enter to select" "↑/↓ to navigate"
                                      "Do you want to proceed?")
  "List of strings indicating Claude is waiting for interactive user input.
Matched against the last 8 non-empty pane lines. Checked BEFORE
active-indicators — these patterns are unambiguously blocked states even
when the active spinner is simultaneously visible.

\"Enter to select\" and \"↑/↓ to navigate\" appear in Claude Code's interactive
selection/choice prompt UI.

\"Do you want to proceed?\" appears in Claude Code's tool-use permission
dialog. As of Claude Code's new UI layout, the permission prompt renders above
the active spinner, so without this check the spinner would cause a false
active reading while actually waiting for permission.

NOTE: do not add \"Esc to cancel\" here — it appears in the footer during both
tool execution (interruptible) and permission prompts, making it ambiguous.
NOTE: do not add \"accept edits\" here — it is a mode indicator, not a blocking
condition."
  :type '(repeat string)
  :group 'bebop)

(defcustom bebop-dashboard-height 10
  "Number of lines to give the dashboard window in the split layout."
  :type 'integer
  :group 'bebop)

(defcustom bebop-gone-threshold 2
  "Consecutive pane capture failures required before considering a pair orphaned."
  :type 'integer
  :group 'bebop)

(defcustom bebop-pane-miss-threshold 3
  "Consecutive pane-id misses required before considering a pair orphaned."
  :type 'integer
  :group 'bebop)

(defface bebop-session-face
  '((t :inherit default))
  "Face for non-selected session lines in the dashboard."
  :group 'bebop)

(defface bebop-selected-face
  '((t :weight bold :inverse-video t))
  "Face for the currently active session line in the dashboard."
  :group 'bebop)

(defface bebop-dot-waiting-face
  '((t :foreground "#CC3333"))
  "Face for the status dot when Claude is waiting for user input."
  :group 'bebop)

(defface bebop-dot-active-face
  '((t :foreground "#33AA44"))
  "Face for the status dot when Claude is active or responding."
  :group 'bebop)

(defface bebop-dot-degraded-face
  '((t :foreground "#6C6C6C"))
  "Face for the status dot when session status is degraded/unknown."
  :group 'bebop)

(defvar bebop--live-sessions nil
  "Alist of (NAME . PINFO) for all known agent pairs.
PINFO is a plist with keys :window :status :pane-id.")

(defvar bebop--active-session nil
  "Name string of the currently selected agent pair, or nil.")

(defvar bebop--poll-timer nil
  "Timer object for periodic status polling.")

(defvar bebop--last-status-details nil
  "Alist of (SESSION-NAME . plist) with recent status inference details.")

(defun bebop--tmux-window-name (pair-name)
  "Return the tmux window name for PAIR-NAME.
If PAIR-NAME starts with /, the window name is everything after the /."
  (if (string-prefix-p "/" pair-name)
      (substring pair-name 1)
    pair-name))

(defun bebop--tmux (&rest args)
  "Run tmux with ARGS via call-process. Return exit code."
  (apply #'call-process "tmux" nil nil nil args))

(defun bebop--tmux-output (&rest args)
  "Run tmux with ARGS and return stdout as a trimmed string, or nil on error."
  (with-temp-buffer
    (let ((code (apply #'call-process "tmux" nil t nil args)))
      (when (eq code 0)
        (string-trim (buffer-string))))))

(defun bebop--tmux-session-exists-p ()
  "Return non-nil if the claude tmux session exists."
  (eq 0 (bebop--tmux "has-session" "-t" bebop-tmux-session)))

(defun bebop--tmux-window-list ()
  "Return list of window name strings in the claude tmux session.
Returns nil if the session does not exist."
  (when (bebop--tmux-session-exists-p)
    (let ((out (bebop--tmux-output
                "list-windows" "-t" bebop-tmux-session
                "-F" "#{window_name}")))
      (when out
        (split-string out "\n" t)))))

(defun bebop--tmux-pane-id-for (name)
  "Return pane id string (e.g. \"%3\") for window NAME in the claude session.
Returns nil if the window does not exist or the pane id is invalid."
  (let* ((target (format "%s:%s" bebop-tmux-session name))
         (out (bebop--tmux-output
               "list-panes" "-t" target "-F" "#{pane_id}")))
    (when (and out (string-match-p "^%[0-9]+$" out))
      out)))

(defun bebop--tmux-capture-pane (pane-id)
  "Return the last 30 lines of PANE-ID output as a string, or nil on error."
  (bebop--tmux-output
   "capture-pane" "-p" "-t" pane-id "-S" "-30"))

(defconst bebop--ansi-re
  (concat "\x1b\\[[0-9;?]*[A-Za-z@]"   ; CSI sequences
          "\\|\x1b][^\x07]*\x07"        ; OSC sequences
          "\\|\r"                        ; carriage returns
          "\\|[\x00\x80-\x9f]")         ; C1 controls + null bytes
  "Combined regex for all terminal escape sequences stripped from pane output.")

(defun bebop--strip-ansi (s)
  "Remove ANSI escape sequences and terminal control bytes from string S."
  (replace-regexp-in-string bebop--ansi-re "" s))

(defun bebop--infer-status (pane-id)
  "Return status symbol for the session owning PANE-ID."
  (plist-get (bebop--infer-status-detail pane-id) :status))

(defun bebop--infer-status-detail (pane-id)
  "Return status detail plist for the session owning PANE-ID.
`gone'      means the pane no longer exists (tmux capture-pane failed).
`blocked'   means Claude needs user action (permission prompt or edit acceptance).
`waiting'   means Claude finished its turn and is at the input prompt.
`active'    means Claude is generating output or thinking.
`unknown'   means pane output exists but confidence is low."
  (let* ((raw (bebop--tmux-capture-pane pane-id))
         (clean (and raw (bebop--strip-ansi raw)))
         (lines (and clean
                     (seq-filter (lambda (l)
                                   (not (string-empty-p (string-trim l))))
                                 (split-string clean "\n"))))
         (tail (and lines (last lines 8))))
    (cond
     ;; capture-pane failed — pane/window no longer exists.
     ((null raw) (list :status 'gone :reason 'capture-failed))
     ;; Pane exists but no meaningful lines: low confidence.
     ((null tail) (list :status 'unknown :reason 'empty-tail))
     ;; Blocked indicators are checked BEFORE active indicators. The new Claude
     ;; CLI UI renders permission prompts above the spinner, so both can appear
     ;; in the pane simultaneously — blocked must win.
     ((cl-some (lambda (l)
                 (cl-some (lambda (pat) (string-match-p pat l))
                          bebop-blocked-indicators))
               tail)
      (list :status 'blocked :reason 'blocked-indicator))
     ;; Active spinner visible and no blocked indicator → Claude is generating.
     ((cl-some (lambda (l)
                 (cl-some (lambda (pat) (string-match-p pat l))
                          bebop-active-indicators))
               tail)
      (list :status 'active :reason 'active-indicator))
     ;; ❯ prompt visible, no active/blocked indicators → waiting for input.
     ((cl-some (lambda (l) (string-match-p "❯" l)) tail)
      (list :status 'waiting :reason 'prompt-visible))
     (t (list :status 'unknown :reason 'no-known-indicator)))))

(defun bebop--spawn-session (name)
  "Create a new agent pair named NAME.
If NAME starts with /, the portion after / is used as the tmux window name
and Claude is started in `bebop-repos-dir'/<rest>.  The full NAME
is used as the dashboard display name and alist key.
Creates a tmux window in the claude session, launches Claude CLI,
and registers the pair in the dashboard."
  (when (string-empty-p (string-trim name))
    (user-error "Agent name cannot be empty"))
  (when (assoc name bebop--live-sessions)
    (user-error "Pair \"%s\" already exists" name))
  (let* ((window-name (bebop--tmux-window-name name))
         (work-dir (when (string-prefix-p "/" name)
                     (expand-file-name window-name bebop-repos-dir))))
    (when (member window-name (bebop--tmux-window-list))
      (user-error "tmux window \"%s\" already exists in session \"%s\""
                  window-name bebop-tmux-session))
    (when (and work-dir (not (file-directory-p work-dir)))
      (user-error "Directory does not exist: %s" work-dir))
    ;; Ensure the tmux session exists
    (unless (bebop--tmux-session-exists-p)
      (bebop--tmux "new-session" "-d" "-s" bebop-tmux-session))
    ;; Create window and start Claude
    (let ((target (format "%s:%s" bebop-tmux-session window-name))
          (env (format "BEBOP_SESSION=%s" (shell-quote-argument name))))
      (bebop--tmux "new-window" "-t" bebop-tmux-session "-n" window-name "-a")
      (if work-dir
          (bebop--tmux "send-keys" "-t" target
                                 (format "cd %s && %s claude"
                                         (shell-quote-argument work-dir) env)
                                 "Enter")
        (bebop--tmux "send-keys" "-t" target (format "%s claude" env) "Enter"))
      ;; Register pair (pane-id may not be available instantly; poll will resolve it)
      (push (cons name (list :window target :status 'unknown
                             :pane-misses 0
                             :capture-failures 0
                             :pane-id (bebop--tmux-pane-id-for window-name)))
            bebop--live-sessions)
      ;; Auto-select if this is the first pair
      (when (null bebop--active-session)
        (bebop-select-session name))
      (bebop--render)
      (message "Spawned agent pair: %s" name))))

(defun bebop--kill-live-session (name)
  "Kill the agent pair named NAME: removes it from state and kills the tmux window."
  (let ((info (cdr (assoc name bebop--live-sessions))))
    (unless info
      (user-error "Unknown pair: %s" name))
    ;; Kill the tmux window (ignore errors if already gone)
    (ignore-errors
      (bebop--tmux "kill-window" "-t" (plist-get info :window))))
  ;; Remove from state
  (setq bebop--live-sessions
        (cl-remove-if (lambda (p) (equal (car p) name))
                      bebop--live-sessions))
  ;; Update active pair if we just killed it
  (when (equal bebop--active-session name)
    (setq bebop--active-session (caar bebop--live-sessions))
    (bebop--apply-active-session))
  (bebop--render)
  (message "Killed agent pair: %s" name))

(defun bebop-select-session (name)
  "Select NAME as the active agent pair."
  (unless (assoc name bebop--live-sessions)
    (user-error "Unknown pair: %s" name))
  (setq bebop--active-session name)
  (bebop--apply-active-session))

(defun bebop--apply-active-session ()
  "Propagate `bebop--active-session' to all dependent state."
  (if (null bebop--active-session)
      (progn
        (setq bebop-tmux-target (format "%s:???" bebop-tmux-session))
        (bebop--update-composition-header))
    (let* ((info (cdr (assoc bebop--active-session bebop--live-sessions)))
           (window (plist-get info :window)))
      ;; Update the global tmux target
      (setq bebop-tmux-target window)
      ;; Switch the tmux window so the terminal follows
      (bebop--tmux "select-window" "-t" window)
      ;; Refresh the composition buffer header
      (bebop--update-composition-header)
      ;; Switch the conductor's composition window to the new session's buffer
      (let* ((conductor (cl-find-if (lambda (f) (frame-parameter f 'bebop-conductor))
                                    (frame-list)))
             (comp-win  (and conductor
                             (frame-parameter conductor 'bebop-comp-window)))
             (comp-buf  (get-buffer (format "*bebop-session: %s*" bebop--active-session))))
        (when (and (window-live-p comp-win) comp-buf)
          (set-window-buffer comp-win comp-buf)))
      ;; Redraw dashboard
      (bebop--render))))

(defun bebop--update-composition-header ()
  "Update the composition buffer header to reflect the active session name."
  (let ((buf (get-buffer bebop-composition-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq bebop--header-pair bebop--active-session)
        (bebop--set-header)))))

(defun bebop--handle-orphan (name)
  "Handle a pair whose tmux window no longer exists."
  (message "Bebop: agent pair \"%s\" disappeared (tmux window gone)" name)
  (setq bebop--live-sessions
        (cl-remove-if (lambda (p) (equal (car p) name))
                      bebop--live-sessions))
  (when (equal bebop--active-session name)
    (setq bebop--active-session (caar bebop--live-sessions))
    (bebop--apply-active-session)))

(defun bebop--discover-existing-sessions ()
  "Populate `bebop--live-sessions' from existing tmux windows.
Skips the window named \"cli\" (the legacy single-agent window) and
backline windows (prefix \"backline--\"), which are venue work shells,
not agent sessions.  Called once on first `bebop-dashboard-open'."
  (dolist (name (bebop--tmux-window-list))
    (unless (or (equal name "cli")
                (string-prefix-p (or (bound-and-true-p bebop-backline-prefix)
                                     "backline--")
                                 name)
                (assoc name bebop--live-sessions))
      (let* ((target (format "%s:%s" bebop-tmux-session name))
             (pane-id (bebop--tmux-pane-id-for name)))
        (push (cons name (list :window target :status 'unknown
                               :pane-id pane-id
                               :pane-misses 0
                               :capture-failures 0))
              bebop--live-sessions))))
  (when (and bebop--live-sessions (null bebop--active-session))
    (setq bebop--active-session (caar bebop--live-sessions))
    (bebop--apply-active-session)))

(defvar bebop--render-body-function #'bebop--render-flat-body
  "Function inserting the dashboard body. bebop-set overrides this
with the setlist tree renderer.")

(defvar bebop-dashboard-footer-lines
  '("n: new  k: kill  RET: select  g: refresh  q: quit")
  "Lines of the dashboard footer cheatsheet. Modules may replace this
list when they add keybindings.")

(defun bebop--render ()
  "Redraw the dashboard buffer with current pair state."
  (let ((buf (get-buffer-create bebop-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'bebop-dashboard-mode)
        (bebop-dashboard-mode))
      (let ((inhibit-read-only t)
            (saved-name (or (get-text-property (point) 'bebop-session-name)
                            (get-text-property (point) 'bebop-on-deck-name))))
        (erase-buffer)
        (funcall bebop--render-body-function)
        ;; Footer
        (insert "\n")
        (when (bound-and-true-p bebop-use-docker)
          (insert (propertize "Docker sandbox: ON  (! to toggle)\n"
                              'face '(:inherit warning :weight bold))))
        (dolist (line bebop-dashboard-footer-lines)
          (insert (propertize (concat line "\n") 'face 'shadow)))
        ;; Restore point to the same entry line (by name, not position).
        (if saved-name
            (let ((found nil))
              (goto-char (point-min))
              (while (and (not found) (< (point) (point-max)))
                (if (or (equal (get-text-property (point) 'bebop-session-name)
                               saved-name)
                        (equal (get-text-property (point) 'bebop-on-deck-name)
                               saved-name))
                    (setq found t)
                  (forward-line 1)))
              (unless found
                (goto-char (point-min))))
          (goto-char (point-min)))))))

(defun bebop--render-flat-body ()
  "Insert the flat pair list — the fallback body when bebop-set is not loaded."
  (if (null bebop--live-sessions)
      (insert (propertize "  No agent pairs. Press 'n' to spawn one.\n"
                          'face 'shadow))
    (dolist (pair bebop--live-sessions)
      (bebop--insert-session-line (car pair) (cdr pair)))))

(defun bebop--insert-session-line (name info)
  "Insert one dashboard line for pair NAME with plist INFO."
  (let* ((status (plist-get info :status))
         (selected (equal name bebop--active-session))
         (line-face (if selected 'bebop-selected-face 'bebop-session-face))
         (dot-face (cond
                    ((memq status '(blocked waiting)) 'bebop-dot-waiting-face)
                    ((memq status '(unknown degraded)) 'bebop-dot-degraded-face)
                    (t 'bebop-dot-active-face)))
         (start (point)))
    ;; Insert pieces separately so the dot keeps its color face.
    ;; Propertizing the whole formatted line at once would overwrite
    ;; the dot's face with line-face.
    (insert (propertize "●" 'face dot-face))
    (insert " ")
    (insert (propertize name 'face line-face))
    (insert "\n")
    ;; bebop-session-name must cover the whole line for navigation to work
    (add-text-properties start (point) (list 'bebop-session-name name))))

(defun bebop--session-at-point ()
  "Return the pair name at point, or nil."
  (get-text-property (point) 'bebop-session-name))

(defun bebop--next-session ()
  "Move point to the next pair line."
  (interactive)
  (let ((start (point)))
    (when (get-text-property (point) 'bebop-session-name)
      (forward-line 1))
    (while (and (< (point) (point-max))
                (null (get-text-property (point) 'bebop-session-name)))
      (forward-line 1))
    (unless (get-text-property (point) 'bebop-session-name)
      (goto-char start))))

(defun bebop--prev-session ()
  "Move point to the previous pair line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (> (point) (point-min))
                (null (get-text-property (point) 'bebop-session-name)))
      (forward-line -1))
    (unless (get-text-property (point) 'bebop-session-name)
      (goto-char start))))

(defun bebop-activate-at-point ()
  "Select the session under point as active and focus the composition buffer."
  (interactive)
  (let ((name (bebop--session-at-point)))
    (if name
        (progn
          (bebop-select-session name)
          (let* ((frame (selected-frame))
                 (comp-win (or (let ((w (frame-parameter frame 'bebop-comp-window)))
                                 (and (window-live-p w) w))
                               (get-buffer-window
                                (get-buffer (format "*bebop-session: %s*" name))))))
            (when comp-win
              (select-window comp-win))))
      (message "No pair at point"))))

(defvar bebop-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")      #'bebop-new-session)
    (define-key map (kbd "RET")    #'bebop-activate-at-point)
    (define-key map (kbd "<down>") #'bebop--next-session)
    (define-key map (kbd "<up>")   #'bebop--prev-session)
    (define-key map (kbd "j")      #'bebop--next-session)
    (define-key map (kbd "p")      #'bebop--prev-session)
    (define-key map (kbd "g")      #'bebop-refresh)
    (define-key map (kbd "D")      #'bebop-debug-pane)
    (define-key map (kbd "q")      #'quit-window)
    map)
  "Keymap for `bebop-dashboard-mode'.")

(define-derived-mode bebop-dashboard-mode magit-section-mode "BebopDash"
  "Major mode for the Bebop multi-agent dashboard.
Derived from `magit-section-mode' for section folding (TAB) with
fold state preserved across poll re-renders via the visibility cache.
\\{bebop-dashboard-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local magit-section-cache-visibility t)
  (setq-local line-spacing -6)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1))
  (setq mode-line-format nil)
  (setq header-line-format
        '((:eval
           (let* ((resolved (and (fboundp 'bebop--resolve-font) (bebop--resolve-font)))
                  (base-h (let ((dh (face-attribute 'default :height nil t)))
                            (if (integerp dh) dh 160)))
                  (abs-h (round (* bebop-header-height base-h))))
             (propertize (concat " " bebop-header-text)
                         'face `(:height ,abs-h :weight bold
                                 :foreground ,bebop-header-color
                                 ,@(when resolved (list :family resolved))))))))
  (face-remap-add-relative 'header-line
                           :background (face-attribute 'default :background nil t)
                           :box nil
                           :underline nil)
  (add-hook 'kill-buffer-hook #'bebop--stop-poll nil t))

(defun bebop--notify-waiting (name)
  "Alert the user that agent pair NAME has finished and is waiting for input."
  (ding t)
  (message "Bebop: %s is waiting for your input" name))

(defun bebop--poll-all ()
  "Poll all known pairs and update their status. Re-render if anything changed."
  (when bebop--live-sessions
    (let ((changed nil)
          (orphans nil))
      (dolist (pair bebop--live-sessions)
        (let* ((name (car pair))
               (info (cdr pair))
               (pane-id (plist-get info :pane-id)))
          (if (null pane-id)
              ;; Try to re-resolve (window may have just started)
              (let ((new-id (bebop--tmux-pane-id-for (bebop--tmux-window-name name))))
                (if new-id
                    (progn (plist-put info :pane-id new-id)
                           (plist-put info :pane-misses 0)
                           (plist-put info :capture-failures 0)
                           (setq changed t))
                  (let* ((misses (1+ (or (plist-get info :pane-misses) 0)))
                         (old-status (plist-get info :status)))
                    (plist-put info :pane-misses misses)
                    (unless (eq old-status 'degraded)
                      (plist-put info :status 'degraded)
                      (setq changed t))
                    ;; Still no pane after threshold — treat as orphan.
                    (when (>= misses bebop-pane-miss-threshold)
                      (push name orphans)
                      (setq changed t)))))
            (let* ((detail (bebop--infer-status-detail pane-id))
                   (new-status (plist-get detail :status))
                   (reason (plist-get detail :reason))
                   (old-status (plist-get info :status)))
              (setf (alist-get name bebop--last-status-details nil nil #'equal)
                    detail)
              (if (eq new-status 'gone)
                  (let ((new-id (bebop--tmux-pane-id-for (bebop--tmux-window-name name)))
                        (failures (1+ (or (plist-get info :capture-failures) 0))))
                    ;; capture-pane can fail transiently; only orphan after threshold.
                    (if new-id
                        (progn
                          (plist-put info :pane-id new-id)
                          (plist-put info :capture-failures 0)
                          (unless (eq old-status 'degraded)
                            (plist-put info :status 'degraded)
                            (setq changed t)))
                      (plist-put info :capture-failures failures)
                      (unless (eq old-status 'degraded)
                        (plist-put info :status 'degraded)
                        (setq changed t))
                      (when (>= failures bebop-gone-threshold)
                        (push name orphans)
                        (setq changed t))))
                (plist-put info :pane-misses 0)
                (plist-put info :capture-failures 0)
                (unless (eq new-status old-status)
                  (plist-put info :status new-status)
                  (setq changed t)
                  (when (memq new-status '(waiting blocked))
                    (bebop--notify-waiting name)))
                (when (and (eq new-status 'unknown)
                           (not (eq old-status 'unknown)))
                  (message "Bebop: %s status uncertain (%s)" name reason)))))))
      ;; Handle orphans after iteration completes to avoid mutating the list mid-loop
      (dolist (name orphans)
        (bebop--handle-orphan name))
      (when changed
        (bebop--render)))))

(defun bebop-refresh ()
  "Manually trigger a full resync: rediscover pairs, poll status, re-render.
Also restarts the poll timer if it stopped."
  (interactive)
  (bebop--discover-existing-sessions)
  (bebop--poll-all)
  (bebop--render)
  (bebop--start-poll))

(defun bebop--start-poll ()
  "Start the status polling timer if not already running."
  (unless (and bebop--poll-timer
               (timerp bebop--poll-timer))
    (setq bebop--poll-timer
          (run-at-time bebop-poll-interval
                       bebop-poll-interval
                       #'bebop--poll-all))))

(defun bebop--stop-poll ()
  "Stop the status polling timer."
  (when (timerp bebop--poll-timer)
    (cancel-timer bebop--poll-timer)
    (setq bebop--poll-timer nil)))

(defun bebop-debug-pane ()
  "Show captured pane content and inferred status for all pairs in *bebop-debug*."
  (let ((buf (get-buffer-create "*bebop-debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (if (null bebop--live-sessions)
          (insert "No known pairs.\n")
        (dolist (pair bebop--live-sessions)
          (let* ((name (car pair))
                 (info (cdr pair))
                 (pane-id (plist-get info :pane-id))
                 (stored   (plist-get info :status))
                 (detail   (and pane-id (bebop--infer-status-detail pane-id)))
                 (inferred (and detail (plist-get detail :status)))
                 (reason   (and detail (plist-get detail :reason)))
                 (raw      (and pane-id (bebop--tmux-capture-pane pane-id)))
                 (clean    (and raw (bebop--strip-ansi raw)))
                 (tail     (and clean
                                (last (seq-filter
                                       (lambda (l) (not (string-empty-p (string-trim l))))
                                       (split-string clean "\n"))
                                      8))))
            (insert (format "=== %s  pane:%s  stored:%s  inferred:%s  reason:%s  misses:%s  capture-failures:%s ===\n"
                            name (or pane-id "nil") stored inferred reason
                            (or (plist-get info :pane-misses) 0)
                            (or (plist-get info :capture-failures) 0)))
            (if tail
                (progn (insert "Last 8 non-empty lines:\n")
                       (dolist (l tail)
                         (insert (format "  %S\n" l))))
              (insert "  (no pane output)\n"))
            (insert "\n")))))
    (display-buffer buf)))

(defun bebop-dashboard-open ()
  "Open the Bebop dashboard and composition buffer split layout.
Dashboard occupies the top of the current window; composition buffer the bottom.
Other windows in the frame are left untouched."
  (interactive)
  ;; Top: dashboard
  (let ((dash-win (selected-window))
        (total-height (window-total-height)))
    (switch-to-buffer (get-buffer-create bebop-buffer-name))
    (unless (eq major-mode 'bebop-dashboard-mode)
      (bebop-dashboard-mode))
    ;; Bottom: composition buffer
    (let ((compose-win (split-window-below)))
      (select-window compose-win)
      (switch-to-buffer (get-buffer-create bebop-composition-buffer-name))
      (unless (eq major-mode 'text-mode) (text-mode))
      (bebop--apply-buffer-settings)
      (local-set-key (kbd "C-c C-c") #'bebop-send-buffer)
      (local-set-key (kbd "M-a") #'bebop-passthrough)
      (bebop--set-header))
    ;; Resize dashboard to top quarter
    (select-window dash-win)
    (let* ((target (max 3 (/ total-height 4)))
           (delta (- target (window-height dash-win))))
      (window-resize dash-win delta)))
  ;; Discover any existing tmux pairs
  (bebop--discover-existing-sessions)
  ;; Start polling
  (bebop--start-poll)
  ;; Initial render
  (bebop--poll-all)
  (bebop--render))

(provide 'bebop-dashboard)

;;; bebop-dashboard.el ends here
