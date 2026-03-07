;;; chant-dashboard.el --- Multi-agent dashboard for Claude tmux panes -*- lexical-binding: t; -*-

;; Usage:
;;   M-x chant-dashboard-open   open the dashboard + chant split layout
;;   chant (in eshell)          same
;;
;; The dashboard lists all active Claude agent pairs with status indicators.
;; Red dot = Claude needs your input to continue (permission prompt or question).
;; Green dot = Claude is active/responding or idle at the prompt.

(require 'claude-chant)

(defcustom chant-dashboard-poll-interval 3
  "Seconds between tmux pane status polls."
  :type 'integer
  :group 'claude-chant)

(defcustom chant-dashboard-session "claude"
  "Name of the tmux session used for all agent windows."
  :type 'string
  :group 'claude-chant)

(defcustom chant-dashboard-buffer-name "*chant-dashboard*"
  "Buffer name for the agent dashboard."
  :type 'string
  :group 'claude-chant)

(defcustom chant-dashboard-active-indicators '("esc to inter" "Symbioting" "✢")
  "List of strings that indicate Claude is actively generating output.
Checked before blocked-indicators. Matched against the last 8 non-empty pane
lines. \"esc to inter\" matches the status bar even when truncated; \"Symbioting\"
matches the pane content when Claude is thinking; \"✢\" matches the task-execution
spinner when Claude is running a plan."
  :type '(repeat string)
  :group 'claude-chant)

(defcustom chant-dashboard-blocked-indicators '("Esc to cancel")
  "List of status bar strings that indicate Claude needs user action.
Matched against the last 8 non-empty pane lines.
NOTE: do not add \"accept edits\" here — that text appears in the status bar
whenever auto-accept-edits mode is on, regardless of whether the agent is
actively generating or idle. It is a mode indicator, not a blocking condition."
  :type '(repeat string)
  :group 'claude-chant)

(defcustom chant-dashboard-height 10
  "Number of lines to give the dashboard window in the split layout."
  :type 'integer
  :group 'claude-chant)

(defcustom chant-dashboard-repos-dir "~/Code/Repos/"
  "Base directory for path-scoped agents.
When an agent name starts with /, the portion after / is appended to this
directory to determine the working directory for Claude."
  :type 'string
  :group 'claude-chant)

(defface chant-dashboard-header-face
  '((t :height 3.0 :weight bold))
  "Face for the dashboard buffer header text."
  :group 'claude-chant)

(defface chant-dashboard-pair-face
  '((t :inherit default))
  "Face for non-selected pair lines in the dashboard."
  :group 'claude-chant)

(defface chant-dashboard-selected-face
  '((t :weight bold :inverse-video t))
  "Face for the currently active pair line in the dashboard."
  :group 'claude-chant)

(defface chant-dashboard-dot-waiting-face
  '((t :foreground "#CC3333"))
  "Face for the status dot when Claude is waiting for user input."
  :group 'claude-chant)

(defface chant-dashboard-dot-active-face
  '((t :foreground "#33AA44"))
  "Face for the status dot when Claude is active or responding."
  :group 'claude-chant)

(defface chant-dashboard-dot-gathering-face
  '((t :foreground "#D4A017"))
  "Face for the status dot when a Bebop session is in gathering mode (pre-first-jam)."
  :group 'claude-chant)

(defvar chant-dashboard--pairs nil
  "Alist of (NAME . PINFO) for all known agent pairs.
PINFO is a plist with keys :window :status :pane-id.")
(setq chant-dashboard--pairs nil)

(defvar chant-dashboard--active-pair nil
  "Name string of the currently selected agent pair, or nil.")
(setq chant-dashboard--active-pair nil)

(defvar chant-dashboard--poll-timer nil
  "Timer object for periodic status polling.")

(defvar-local chant-dashboard--header-overlay nil
  "Overlay used for the dashboard buffer header.")

(defun chant-dashboard--tmux-window-name (pair-name)
  "Return the tmux window name for PAIR-NAME.
If PAIR-NAME starts with /, the window name is everything after the /."
  (if (string-prefix-p "/" pair-name)
      (substring pair-name 1)
    pair-name))

(defun chant-dashboard--tmux (&rest args)
  "Run tmux with ARGS via call-process. Return exit code."
  (apply #'call-process "tmux" nil nil nil args))

(defun chant-dashboard--tmux-output (&rest args)
  "Run tmux with ARGS and return stdout as a trimmed string, or nil on error."
  (with-temp-buffer
    (let ((code (apply #'call-process "tmux" nil t nil args)))
      (when (eq code 0)
        (string-trim (buffer-string))))))

(defun chant-dashboard--session-exists-p ()
  "Return non-nil if the claude tmux session exists."
  (eq 0 (chant-dashboard--tmux "has-session" "-t" chant-dashboard-session)))

(defun chant-dashboard--window-list ()
  "Return list of window name strings in the claude tmux session.
Returns nil if the session does not exist."
  (when (chant-dashboard--session-exists-p)
    (let ((out (chant-dashboard--tmux-output
                "list-windows" "-t" chant-dashboard-session
                "-F" "#{window_name}")))
      (when out
        (split-string out "\n" t)))))

(defun chant-dashboard--pane-id-for (name)
  "Return pane id string (e.g. \"%3\") for window NAME in the claude session.
Returns nil if the window does not exist or the pane id is invalid."
  (let* ((target (format "%s:%s" chant-dashboard-session name))
         (out (chant-dashboard--tmux-output
               "list-panes" "-t" target "-F" "#{pane_id}")))
    (when (and out (string-match-p "^%[0-9]+$" out))
      out)))

(defun chant-dashboard--capture-pane (pane-id)
  "Return the last 30 lines of PANE-ID output as a string, or nil on error."
  (chant-dashboard--tmux-output
   "capture-pane" "-p" "-t" pane-id "-S" "-30"))

(defun chant-dashboard--strip-ansi (s)
  "Remove ANSI escape sequences and terminal control bytes from string S."
  (let* ((s (replace-regexp-in-string "\x1b\\[[0-9;?]*[A-Za-z@]" "" s))
         (s (replace-regexp-in-string "\x1b][^\x07]*\x07" "" s))
         (s (replace-regexp-in-string "\r" "" s))
         ;; Strip C1 control bytes (0x80-0x9F) and null bytes used as TUI padding
         (s (replace-regexp-in-string "[\x00\x80-\x9f]" "" s)))
    s))

(defun chant-dashboard--infer-status (pane-id)
  "Return status symbol for the session owning PANE-ID.
`gathering' means the session is in Bebop gathering mode (pre-first-jam).
`gone'      means the pane no longer exists (tmux capture-pane failed).
`blocked'   means Claude needs user action (permission prompt or edit acceptance).
`waiting'   means Claude finished its turn and is at the input prompt.
`active'    means Claude is generating output or thinking."
  (or
   ;; Check Bebop gathering state first — no pane read needed.
   ;; Reverse-look up the session name by matching pane-id in the pairs alist.
   (let ((name (car (cl-find-if
                      (lambda (p)
                        (equal (plist-get (cdr p) :pane-id) pane-id))
                      chant-dashboard--pairs))))
     (when (and name
                (fboundp 'bebop--pair-gathering-p)
                (bebop--pair-gathering-p name))
       'gathering))
   ;; Normal pane-content detection.
   (let* ((raw (chant-dashboard--capture-pane pane-id))
          (clean (and raw (chant-dashboard--strip-ansi raw)))
          (lines (and clean
                      (seq-filter (lambda (l)
                                    (not (string-empty-p (string-trim l))))
                                  (split-string clean "\n"))))
          (tail (and lines (last lines 8))))
     (cond
      ;; capture-pane failed — pane/window no longer exists.
      ((null raw) 'gone)
      ;; Pane or status bar shows an active-generation indicator → Claude is generating.
      ;; Checked before blocked-indicators so "accept edits on" + "esc to inter"
      ;; (auto-accept mode enabled while generating) doesn't trip the blocked check.
      ((cl-some (lambda (l)
                  (cl-some (lambda (pat) (string-match-p pat l))
                           chant-dashboard-active-indicators))
                tail)
       'active)
      ;; Status bar shows a permission or edit-acceptance prompt → needs user action.
      ((cl-some (lambda (l)
                  (cl-some (lambda (pat) (string-match-p pat l))
                           chant-dashboard-blocked-indicators))
                tail)
       'blocked)
      ;; ❯ prompt visible, no active/blocked indicators → waiting for input.
      ((cl-some (lambda (l) (string-match-p "❯" l)) tail)
       'waiting)
      (t 'active)))))

(defun chant-dashboard-new-pair (name)
  "Create a new agent pair named NAME.
If NAME starts with /, the portion after / is used as the tmux window name
and Claude is started in `chant-dashboard-repos-dir'/<rest>.  The full NAME
is used as the dashboard display name and alist key.
Creates a tmux window in the claude session, launches Claude CLI,
and registers the pair in the dashboard."
  (interactive "sAgent name: ")
  (when (string-empty-p (string-trim name))
    (user-error "Agent name cannot be empty"))
  (when (assoc name chant-dashboard--pairs)
    (user-error "Pair \"%s\" already exists" name))
  (let* ((window-name (chant-dashboard--tmux-window-name name))
         (work-dir (when (string-prefix-p "/" name)
                     (expand-file-name window-name chant-dashboard-repos-dir))))
    (when (member window-name (chant-dashboard--window-list))
      (user-error "tmux window \"%s\" already exists in session \"%s\""
                  window-name chant-dashboard-session))
    (when (and work-dir (not (file-directory-p work-dir)))
      (user-error "Directory does not exist: %s" work-dir))
    ;; Ensure the tmux session exists
    (unless (chant-dashboard--session-exists-p)
      (chant-dashboard--tmux "new-session" "-d" "-s" chant-dashboard-session))
    ;; Create window and start Claude
    (let ((target (format "%s:%s" chant-dashboard-session window-name)))
      (chant-dashboard--tmux "new-window" "-t" chant-dashboard-session "-n" window-name "-a")
      (if work-dir
          (chant-dashboard--tmux "send-keys" "-t" target
                                 (format "cd %s && claude" (shell-quote-argument work-dir))
                                 "Enter")
        (chant-dashboard--tmux "send-keys" "-t" target "claude" "Enter"))
      ;; Register pair (pane-id may not be available instantly; poll will resolve it)
      (push (cons name (list :window target :status 'active
                             :pane-id (chant-dashboard--pane-id-for window-name)))
            chant-dashboard--pairs)
      ;; Auto-select if this is the first pair
      (when (null chant-dashboard--active-pair)
        (chant-dashboard-select-pair name))
      (chant-dashboard--render)
      (message "Spawned agent pair: %s" name))))

(defun chant-dashboard-kill-pair (name)
  "Kill the agent pair named NAME: removes it from state and kills the tmux window."
  (interactive
   (list (or (and chant-dashboard--pairs
                  (completing-read "Kill pair: "
                                   (mapcar #'car chant-dashboard--pairs)
                                   nil t))
             (user-error "No pairs to kill"))))
  (let ((info (cdr (assoc name chant-dashboard--pairs))))
    (unless info
      (user-error "Unknown pair: %s" name))
    ;; Kill the tmux window (ignore errors if already gone)
    (ignore-errors
      (chant-dashboard--tmux "kill-window" "-t" (plist-get info :window))))
  ;; Remove from state
  (setq chant-dashboard--pairs
        (cl-remove-if (lambda (p) (equal (car p) name))
                      chant-dashboard--pairs))
  ;; Update active pair if we just killed it
  (when (equal chant-dashboard--active-pair name)
    (setq chant-dashboard--active-pair (caar chant-dashboard--pairs))
    (chant-dashboard--apply-active-pair))
  (chant-dashboard--render)
  (message "Killed agent pair: %s" name))

(defun chant-dashboard-select-pair (name)
  "Select NAME as the active agent pair."
  (unless (assoc name chant-dashboard--pairs)
    (user-error "Unknown pair: %s" name))
  (setq chant-dashboard--active-pair name)
  (chant-dashboard--apply-active-pair))

(defun chant-dashboard--apply-active-pair ()
  "Propagate `chant-dashboard--active-pair' to all dependent state."
  (if (null chant-dashboard--active-pair)
      (progn
        (setq claude-chant-target (format "%s:???" chant-dashboard-session))
        (chant-dashboard--update-chant-header))
    (let* ((info (cdr (assoc chant-dashboard--active-pair chant-dashboard--pairs)))
           (window (plist-get info :window)))
      ;; Update the global chant target
      (setq claude-chant-target window)
      ;; Switch the tmux window so the terminal follows
      (chant-dashboard--tmux "select-window" "-t" window)
      ;; Refresh the chant buffer header
      (chant-dashboard--update-chant-header)
      ;; Redraw dashboard
      (chant-dashboard--render))))

(defun chant-dashboard--update-chant-header ()
  "Update the chant buffer header to reflect the active pair name."
  (let ((buf (get-buffer claude-chant-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq claude-chant--header-pair chant-dashboard--active-pair)
        (claude-chant--set-header)))))

(defun chant-dashboard--handle-orphan (name)
  "Handle a pair whose tmux window no longer exists."
  (message "Chant: agent pair \"%s\" disappeared (tmux window gone)" name)
  (setq chant-dashboard--pairs
        (cl-remove-if (lambda (p) (equal (car p) name))
                      chant-dashboard--pairs))
  (when (equal chant-dashboard--active-pair name)
    (setq chant-dashboard--active-pair (caar chant-dashboard--pairs))
    (chant-dashboard--apply-active-pair)))

(defun chant-dashboard--discover-existing-pairs ()
  "Populate `chant-dashboard--pairs' from existing tmux windows.
Skips the window named \"cli\" (the legacy single-agent window).
Called once on first `chant-dashboard-open'."
  (dolist (name (chant-dashboard--window-list))
    (unless (or (equal name "cli")
                (assoc name chant-dashboard--pairs))
      (let* ((target (format "%s:%s" chant-dashboard-session name))
             (pane-id (chant-dashboard--pane-id-for name)))
        (push (cons name (list :window target :status 'active :pane-id pane-id))
              chant-dashboard--pairs))))
  (when (and chant-dashboard--pairs (null chant-dashboard--active-pair))
    (setq chant-dashboard--active-pair (caar chant-dashboard--pairs))
    (chant-dashboard--apply-active-pair)))

(defun chant-dashboard--render ()
  "Redraw the dashboard buffer with current pair state."
  (let ((buf (get-buffer-create chant-dashboard-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (saved-point (point)))
        (erase-buffer)
        ;; Header — inserted as buffer text so pair-line faces don't bleed into it.
        ;; (A before-string overlay at point-min shares face context with whatever
        ;; starts at position 1, so :inverse-video on the first pair would highlight
        ;; the header too.)
        (when (overlayp chant-dashboard--header-overlay)
          (delete-overlay chant-dashboard--header-overlay)
          (setq chant-dashboard--header-overlay nil))
        ;; Pair lines
        (if (null chant-dashboard--pairs)
            (insert (propertize "  No agent pairs. Press 'n' to spawn one.\n"
                                'face 'shadow))
          (dolist (pair chant-dashboard--pairs)
            (chant-dashboard--insert-pair-line (car pair) (cdr pair))))
        ;; Footer
        (insert (propertize "\n  n: new  k: kill  r: rename  M-↑/↓: move  RET: select  g: refresh  q: quit\n"
                            'face 'shadow))
        ;; Restore point within bounds
        (goto-char (min saved-point (point-max)))))))

(defun chant-dashboard--insert-pair-line (name info)
  "Insert one dashboard line for pair NAME with plist INFO."
  (let* ((status (plist-get info :status))
         (selected (equal name chant-dashboard--active-pair))
         (line-face (if selected 'chant-dashboard-selected-face 'chant-dashboard-pair-face))
         (dot-face (cond
                    ((eq status 'gathering) 'chant-dashboard-dot-gathering-face)
                    ((memq status '(blocked waiting)) 'chant-dashboard-dot-waiting-face)
                    (t 'chant-dashboard-dot-active-face)))
         (start (point)))
    ;; Insert pieces separately so the dot keeps its color face.
    ;; Propertizing the whole formatted line at once would overwrite
    ;; the dot's face with line-face.
    (insert "  ")
    (insert (propertize "●" 'face dot-face))
    (insert "  ")
    (insert (propertize name 'face line-face))
    (insert "\n")
    ;; chant-pair-name must cover the whole line for navigation to work
    (add-text-properties start (point) (list 'chant-pair-name name))))

(defun chant-dashboard--pair-at-point ()
  "Return the pair name at point, or nil."
  (get-text-property (point) 'chant-pair-name))

(defun chant-dashboard-next-pair ()
  "Move point to the next pair line."
  (interactive)
  (let ((start (point)))
    (when (get-text-property (point) 'chant-pair-name)
      (forward-line 1))
    (while (and (< (point) (point-max))
                (null (get-text-property (point) 'chant-pair-name)))
      (forward-line 1))
    (unless (get-text-property (point) 'chant-pair-name)
      (goto-char start))))

(defun chant-dashboard-prev-pair ()
  "Move point to the previous pair line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (> (point) (point-min))
                (null (get-text-property (point) 'chant-pair-name)))
      (forward-line -1))
    (unless (get-text-property (point) 'chant-pair-name)
      (goto-char start))))

(defun chant-dashboard-activate-at-point ()
  "Select the pair under point as the active pair and focus the chant buffer."
  (interactive)
  (let ((name (chant-dashboard--pair-at-point)))
    (if name
        (progn
          (chant-dashboard-select-pair name)
          (let ((chant-win (get-buffer-window claude-chant-buffer-name)))
            (when chant-win
              (select-window chant-win))))
      (message "No pair at point"))))

(defun chant-dashboard-kill-at-point ()
  "Kill the pair under point. Requires typing the agent name to confirm."
  (interactive)
  (let ((name (chant-dashboard--pair-at-point)))
    (if (null name)
        (message "No pair at point")
      (let ((typed (read-string (format "Type \"%s\" to confirm kill: " name))))
        (if (string= typed name)
            (chant-dashboard-kill-pair name)
          (message "Cancelled (name mismatch)."))))))

(defun chant-dashboard--move-pair (name direction)
  "Move pair NAME one step in DIRECTION (:up or :down) in `chant-dashboard--pairs'."
  (let ((idx (cl-position name chant-dashboard--pairs :key #'car :test #'equal)))
    (when idx
      (let* ((len      (length chant-dashboard--pairs))
             (swap-idx (if (eq direction :up) (1- idx) (1+ idx))))
        (when (and (>= swap-idx 0) (< swap-idx len))
          (cl-rotatef (nth idx chant-dashboard--pairs)
                      (nth swap-idx chant-dashboard--pairs))
          t)))))

(defun chant-dashboard--goto-pair (name)
  "Move point to the line for pair NAME in the dashboard buffer."
  (goto-char (point-min))
  (while (and (< (point) (point-max))
              (not (equal (get-text-property (point) 'chant-pair-name) name)))
    (forward-line 1)))

(defun chant-dashboard-move-pair-up ()
  "Move the pair under point one position up in the dashboard."
  (interactive)
  (let ((name (chant-dashboard--pair-at-point)))
    (if (null name)
        (message "No pair at point")
      (when (chant-dashboard--move-pair name :up)
        (chant-dashboard--render)
        (chant-dashboard--goto-pair name)))))

(defun chant-dashboard-move-pair-down ()
  "Move the pair under point one position down in the dashboard."
  (interactive)
  (let ((name (chant-dashboard--pair-at-point)))
    (if (null name)
        (message "No pair at point")
      (when (chant-dashboard--move-pair name :down)
        (chant-dashboard--render)
        (chant-dashboard--goto-pair name)))))

(defun chant-dashboard-rename-at-point ()
  "Rename the session under point."
  (interactive)
  (let ((name (chant-dashboard--pair-at-point)))
    (if (null name)
        (message "No pair at point")
      (let ((new-name (read-string (format "Rename \"%s\" to: " name))))
        (when (string-empty-p (string-trim new-name))
          (user-error "Name cannot be empty"))
        (if (fboundp 'bebop-rename-session)
            (bebop-rename-session name new-name)
          ;; Plain chant pair: update alist and tmux window only
          (when (assoc new-name chant-dashboard--pairs)
            (user-error "A session named \"%s\" already exists" new-name))
          (let* ((old-window (format "%s:%s" chant-dashboard-session name))
                 (new-window (format "%s:%s" chant-dashboard-session new-name))
                 (pair (assoc name chant-dashboard--pairs)))
            (chant-dashboard--tmux "rename-window" "-t" old-window new-name)
            (when pair
              (setcar pair new-name)
              (plist-put (cdr pair) :window new-window))
            (when (equal chant-dashboard--active-pair name)
              (setq chant-dashboard--active-pair new-name)
              (setq claude-chant-target new-window))
            (chant-dashboard--render)
            (message "Renamed \"%s\" → \"%s\"" name new-name)))))))

(defun chant-dashboard-spawn-interactive ()
  "Prompt for a name and spawn a new agent pair.
Delegates to `bebop-new-session' (3-step flow) when Bebop is loaded,
falling back to `chant-dashboard-new-pair' otherwise."
  (interactive)
  (if (fboundp 'bebop-new-session)
      (call-interactively #'bebop-new-session)
    (call-interactively #'chant-dashboard-new-pair)))

(defvar chant-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")        #'chant-dashboard-spawn-interactive)
    (define-key map (kbd "k")        #'chant-dashboard-kill-at-point)
    (define-key map (kbd "r")        #'chant-dashboard-rename-at-point)
    (define-key map (kbd "RET")      #'chant-dashboard-activate-at-point)
    (define-key map (kbd "M-<up>")   #'chant-dashboard-move-pair-up)
    (define-key map (kbd "M-<down>") #'chant-dashboard-move-pair-down)
    (define-key map (kbd "<down>") #'chant-dashboard-next-pair)
    (define-key map (kbd "<up>")   #'chant-dashboard-prev-pair)
    (define-key map (kbd "j")      #'chant-dashboard-next-pair)
    (define-key map (kbd "p")      #'chant-dashboard-prev-pair)
    (define-key map (kbd "g")      #'chant-dashboard-refresh)
    (define-key map (kbd "D")      #'chant-dashboard-debug-pane)
    (define-key map (kbd "q")      #'quit-window)
    map)
  "Keymap for `chant-dashboard-mode'.")

(define-derived-mode chant-dashboard-mode special-mode "ChantDash"
  "Major mode for the Chant multi-agent dashboard.
\\{chant-dashboard-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1))
  (add-hook 'kill-buffer-hook #'chant-dashboard--stop-poll nil t))

(defun chant-dashboard--notify-waiting (name)
  "Alert the user that agent pair NAME has finished and is waiting for input."
  (ding t)
  (message "Chant: %s is waiting for your input" name))

(defun chant-dashboard--poll-all ()
  "Poll all known pairs and update their status. Re-render if anything changed."
  (when chant-dashboard--pairs
    (let ((changed nil)
          (orphans nil))
      (dolist (pair chant-dashboard--pairs)
        (let* ((name (car pair))
               (info (cdr pair))
               (pane-id (plist-get info :pane-id)))
          (if (null pane-id)
              ;; Try to re-resolve (window may have just started)
              (let ((new-id (chant-dashboard--pane-id-for (chant-dashboard--tmux-window-name name))))
                (if new-id
                    (progn (plist-put info :pane-id new-id)
                           (setq changed t))
                  ;; Still no pane — window is gone
                  (push name orphans)
                  (setq changed t)))
            (let ((new-status (chant-dashboard--infer-status pane-id))
                  (old-status (plist-get info :status)))
              (if (eq new-status 'gone)
                  ;; Confirm via window lookup before declaring orphan —
                  ;; capture-pane can fail transiently while a TUI is
                  ;; initialising the alternate screen.
                  (when (null (chant-dashboard--pane-id-for (chant-dashboard--tmux-window-name name)))
                    (push name orphans)
                    (setq changed t))
                (unless (eq new-status old-status)
                  (plist-put info :status new-status)
                  (setq changed t)
                  (when (memq new-status '(waiting blocked))
                    (chant-dashboard--notify-waiting name))))))))
      ;; Handle orphans after iteration completes to avoid mutating the list mid-loop
      (dolist (name orphans)
        (chant-dashboard--handle-orphan name))
      (when changed
        (chant-dashboard--render)))))

(defun chant-dashboard-refresh ()
  "Manually trigger a full resync: rediscover pairs, poll status, re-render.
Also restarts the poll timer if it stopped."
  (interactive)
  (chant-dashboard--discover-existing-pairs)
  (chant-dashboard--poll-all)
  (chant-dashboard--render)
  (chant-dashboard--start-poll))

(defun chant-dashboard--start-poll ()
  "Start the status polling timer if not already running."
  (unless (and chant-dashboard--poll-timer
               (timerp chant-dashboard--poll-timer))
    (setq chant-dashboard--poll-timer
          (run-at-time chant-dashboard-poll-interval
                       chant-dashboard-poll-interval
                       #'chant-dashboard--poll-all))))

(defun chant-dashboard--stop-poll ()
  "Stop the status polling timer."
  (when (timerp chant-dashboard--poll-timer)
    (cancel-timer chant-dashboard--poll-timer)
    (setq chant-dashboard--poll-timer nil)))

(defun chant-dashboard-debug-pane ()
  "Show captured pane content and inferred status for all pairs in *chant-debug*."
  (interactive)
  (let ((buf (get-buffer-create "*chant-debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (if (null chant-dashboard--pairs)
          (insert "No known pairs.\n")
        (dolist (pair chant-dashboard--pairs)
          (let* ((name (car pair))
                 (info (cdr pair))
                 (pane-id (plist-get info :pane-id))
                 (stored   (plist-get info :status))
                 (inferred (and pane-id (chant-dashboard--infer-status pane-id)))
                 (raw      (and pane-id (chant-dashboard--capture-pane pane-id)))
                 (clean    (and raw (chant-dashboard--strip-ansi raw)))
                 (tail     (and clean
                                (last (seq-filter
                                       (lambda (l) (not (string-empty-p (string-trim l))))
                                       (split-string clean "\n"))
                                      8))))
            (insert (format "=== %s  pane:%s  stored:%s  inferred:%s ===\n"
                            name (or pane-id "nil") stored inferred))
            (if tail
                (progn (insert "Last 8 non-empty lines:\n")
                       (dolist (l tail)
                         (insert (format "  %S\n" l))))
              (insert "  (no pane output)\n"))
            (insert "\n")))))
    (display-buffer buf)))

(defun chant-dashboard-open ()
  "Open the Chant dashboard + chant buffer split layout.
Dashboard occupies the top of the current window; chant buffer the bottom.
Other windows in the frame are left untouched."
  (interactive)
  ;; Top: dashboard
  (let ((dash-win (selected-window))
        (total-height (window-total-height)))
    (switch-to-buffer (get-buffer-create chant-dashboard-buffer-name))
    (unless (eq major-mode 'chant-dashboard-mode)
      (chant-dashboard-mode))
    ;; Bottom: chant buffer
    (let ((chant-win (split-window-below)))
      (select-window chant-win)
      (switch-to-buffer (get-buffer-create claude-chant-buffer-name))
      (unless (eq major-mode 'text-mode) (text-mode))
      (claude-chant--apply-buffer-settings)
      (local-set-key (kbd "C-c C-c") #'claude-chant-send-buffer)
      (local-set-key (kbd "M-a") #'abeyance)
      (claude-chant--set-header))
    ;; Resize dashboard to top quarter
    (select-window dash-win)
    (let* ((target (max 3 (/ total-height 4)))
           (delta (- target (window-height dash-win))))
      (window-resize dash-win delta)))
  ;; Discover any existing tmux pairs
  (chant-dashboard--discover-existing-pairs)
  ;; Start polling
  (chant-dashboard--start-poll)
  ;; Initial render
  (chant-dashboard--poll-all)
  (chant-dashboard--render))

(provide 'chant-dashboard)

;;; chant-dashboard.el ends here
