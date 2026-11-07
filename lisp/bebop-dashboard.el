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

(defcustom bebop-sweep-interval 30
  "Seconds between sweeps of every session's tmux pane.
Hook events reach the dashboard as they are written (see Pushed
events), so the sweep only covers what hooks cannot say: a window that
is gone, a session without events, and a lost file watch."
  :type 'integer
  :group 'bebop)

(defcustom bebop-interrupt-check-interval 5
  "Seconds between pane checks of working sessions whose events went quiet.
No hook fires on an Esc interrupt. The transcript watch catches it; this
check covers only sessions whose transcript is not watched."
  :type 'integer
  :group 'bebop)

(defcustom bebop-idle-render-interval 60
  "Seconds between dashboard re-renders when nothing has changed.
The render loop normally fires only on status edges, but the HUD's
age column drifts in real time — without an idle cadence a quiet
dashboard shows \"12m\" forever. Coarse on purpose: ages display at
minute granularity."
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

(defcustom bebop-hud-dot-stale-days 7
  "Days without activity after which a status dot decays to shadow.
The dot-level twin of `bebop-hud-quiet-hours' name dimming: a session
untouched for a week should not glow as loud as one that flipped a
minute ago. No state is lost — the gutter glyphs and `bebop-session-info'
still carry it; only the dot's salience decays."
  :type 'number
  :group 'bebop)

(declare-function bebop-set--last-activity "bebop-set")
(declare-function bebop-set--age-seconds "bebop-set")

(defun bebop--dot-stale-p (name)
  "Non-nil if NAME's last activity predates `bebop-hud-dot-stale-days'.
Nil whenever the answer is unknown — bebop-set unloaded, or a session
never stamped — so the dot keeps its status color until an activity
record proves it stale."
  (when (and (fboundp 'bebop-set--last-activity)
             (fboundp 'bebop-set--age-seconds))
    (when-let ((secs (bebop-set--age-seconds
                      (bebop-set--last-activity name))))
      (> secs (* bebop-hud-dot-stale-days 86400)))))

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

(defvar bebop--last-idle-render 0
  "`float-time' of the last poll-driven render — the idle-render clock.")

(defvar bebop--last-status-details nil
  "Alist of (SESSION-NAME . plist) with recent status inference details.")

(defvar bebop-session-activity-functions nil
  "Abnormal hook run with (NAME EVENT) when a session shows real activity.
EVENT is a symbol: `transition' (the agent crossed between working and
waiting, per the poll loop's edge detection) or `send' (a prompt went
to the agent — composition buffer or jam). Downstream modules subscribe
to keep time; the dashboard itself stores nothing. This is the HUD's
timekeeping signal — see the HUD design section.")

(defvar bebop-session-end-functions nil
  "Abnormal hook run with NAME when a session's runtime ends.
Fires on a pause (`bebop-kill-session') and when the poll finds the
window gone, before the registries drop NAME, so subscribers can still
resolve its venue. Teardown does not fire it: teardown reaps every
facet itself. The backline subscribes to close the ended session's
idle work shell.")

(defvar bebop-session-handoff-functions nil
  "Abnormal hook run with (NAME TEXT) when `bebop-jam' delivers TEXT.
Only jams fire it, never composition sends: a jam is work that a skill,
a sibling agent or a remote machine handed to the session, and the
handoff ledger (bebop-set.el) tracks it until you ack the row.")

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

(defun bebop--ensure-tmux-session ()
  "Create the claude tmux session if it does not exist."
  (unless (bebop--tmux-session-exists-p)
    (bebop--tmux "new-session" "-d" "-s" bebop-tmux-session)))

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

(defcustom bebop-events-dir (expand-file-name "~/.emacs.d-local/bebop-events/")
  "Directory where bin/bebop-hook writes one event file per session.
Machine-local on purpose: the events describe agents on this host."
  :type 'directory
  :group 'bebop)

(defcustom bebop-interrupt-indicators '("Interrupted · What should Claude do instead")
  "Pane text Claude shows after you interrupt a turn with Esc.
No hook fires on an interrupt, so the events keep saying `working'.
This marker, with no `bebop-working-footer' beside it, says the turn
is over."
  :type '(repeat regexp)
  :group 'bebop)

(defcustom bebop-working-footer "esc to interrupt"
  "Footer text Claude shows only while a turn runs.
An idle prompt alone is no evidence of an interrupt: in fullscreen mode
the spinner scrolls out of the captured tail while text streams, but
this footer stays."
  :type 'regexp
  :group 'bebop)

(defvar bebop--hook-states (make-hash-table :test #'equal)
  "Session name → folded hook state.
A plist: :state (\"working\" \"blocked\" \"done\" \"idle\" \"error\", or
nil once the session ended), :kind, :msg, :at (time of the last event),
:children (alist of AGENT-ID → plist :state :kind :msg :title :type),
:pending (descriptions of Agent calls not yet matched to a start) and
:offset (bytes of the event file already read).")

(defun bebop--events-file (name)
  "Return the event file for session NAME, named as bin/bebop-hook names it."
  (expand-file-name
   (concat (replace-regexp-in-string "/" "_" (string-remove-prefix "/" name))
           ".jsonl")
   bebop-events-dir))

(defun bebop--hook-rank (state)
  "Urgency of hook STATE: what needs you outranks what is news."
  (pcase state
    ((or "blocked" "error") 4) ("done" 3) ("working" 2) ("idle" 1) (_ 0)))

(defun bebop--hook-kind (ev)
  "The blocked kind for permission request EV."
  (if (equal (plist-get ev :tool) "AskUserQuestion") "question" "permission"))

(defun bebop--hook-blocked-msg (ev)
  "The message for permission request EV, in Claude's own phrasing."
  (if (equal (plist-get ev :tool) "AskUserQuestion")
      (format "answer: %s" (or (plist-get ev :q) "a question"))
    (format "approve %s: %s" (plist-get ev :tool)
            (or (plist-get ev :cmd) (plist-get ev :desc) ""))))

(defun bebop--hook-child-put (children id props)
  "Return CHILDREN with agent ID's record updated by plist PROPS.
A new child goes at the end, so children keep their start order."
  (let* ((cell (assoc id children))
         (rec (cdr cell)))
    (while props
      (setq rec (plist-put rec (car props) (cadr props))
            props (cddr props)))
    (if cell
        (progn (setcdr cell rec) children)
      (append children (list (cons id rec))))))

(defun bebop--hook-fold (st ev)
  "Fold hook event EV into state ST. Return the new state plist."
  (let ((e (plist-get ev :e))
        (aid (plist-get ev :aid))
        (state (plist-get st :state))
        (kind (plist-get st :kind))
        (msg (plist-get st :msg))
        (children (copy-tree (plist-get st :children)))
        (pending (plist-get st :pending)))
    (if aid
        (pcase e
          ("SubagentStart"
           (setq children (bebop--hook-child-put
                           children aid
                           (list :state "working"
                                 :type (plist-get ev :atype)
                                 :title (or (car pending) (plist-get ev :atype))))
                 pending (cdr pending)))
          ("SubagentStop"
           (setq children (assoc-delete-all aid children)))
          ("PermissionRequest"
           (when (assoc aid children)
             (setq children (bebop--hook-child-put
                             children aid
                             (list :state "blocked"
                                   :kind (bebop--hook-kind ev)
                                   :msg (bebop--hook-blocked-msg ev))))))
          ((or "PreToolUse" "PostToolUse" "PostToolUseFailure")
           (when (assoc aid children)
             (setq children (bebop--hook-child-put
                             children aid (list :state "working" :kind nil :msg nil))))))
      (pcase e
        ("SessionStart"
         (setq state "idle" kind nil msg nil children nil pending nil))
        ("UserPromptSubmit"
         (setq state "working" kind nil msg nil))
        ((or "PreToolUse" "PostToolUse" "PostToolUseFailure")
         (setq state "working" kind nil msg nil)
         (when (and (equal e "PreToolUse")
                    (equal (plist-get ev :tool) "Agent")
                    (plist-get ev :desc))
           (setq pending (append pending (list (plist-get ev :desc)))))
         (when-let ((id (plist-get ev :spawned)))
           (when (assoc id children)
             (setq children (bebop--hook-child-put
                             children id (list :title (plist-get ev :desc)))))))
        ("PermissionRequest"
         (setq state "blocked"
               kind (bebop--hook-kind ev)
               msg (bebop--hook-blocked-msg ev)))
        ("Elicitation"
         (setq state "blocked" kind "question" msg "MCP input"))
        ("Notification"
         ;; Echoes a PermissionRequest already folded. On its own (a
         ;; dialog no other hook saw) it still means the session waits.
         (when (and (equal (plist-get ev :nt) "permission_prompt")
                    (not (equal state "blocked"))
                    (not (cl-some (lambda (c)
                                    (equal (plist-get (cdr c) :state) "blocked"))
                                  children)))
           (setq state "blocked" kind "permission" msg nil)))
        ("Stop"
         (let ((bg (plist-get ev :bg)))
           (setq state "done" kind nil msg nil pending nil
                 children (cl-remove-if-not
                           (lambda (c)
                             (cl-find (car c) bg
                                      :key (lambda (b) (plist-get b :id))
                                      :test #'equal))
                           children))
           (dolist (b bg)
             (setq children (bebop--hook-child-put
                             children (plist-get b :id)
                             (list :title (plist-get b :desc)
                                   :type (plist-get b :atype)
                                   :state (or (plist-get
                                               (cdr (assoc (plist-get b :id) children))
                                               :state)
                                              "working")))))))
        ("StopFailure"
         (setq state "error" kind nil msg (plist-get ev :err)))
        ("SessionEnd"
         (setq state nil kind nil msg nil children nil pending nil))))
    (list :state state :kind kind :msg msg :children children :pending pending
          :at (/ (or (plist-get ev :t) 0) 1000.0)
          :transcript (or (plist-get ev :tp) (plist-get st :transcript))
          :offset (plist-get st :offset))))

(defun bebop--hook-state (name)
  "Return the folded hook state of session NAME, reading new events first.
Nil when the session has no event file: hooks are not installed, or the
session predates them."
  (let* ((file (bebop--events-file name))
         (size (file-attribute-size (file-attributes file))))
    (when size
      (let* ((st (gethash name bebop--hook-states))
             (offset (or (plist-get st :offset) 0)))
        (when (< size offset)
          (setq st nil offset 0))
        (when (> size offset)
          (with-temp-buffer
            (insert-file-contents-literally file nil offset size)
            (goto-char (point-max))
            (when (search-backward "\n" nil t)
              (let ((end (1+ (point))))
                (dolist (line (split-string
                               (buffer-substring-no-properties (point-min) end)
                               "\n" t))
                  (when-let ((ev (ignore-errors
                                   (json-parse-string
                                    (decode-coding-string line 'utf-8)
                                    :object-type 'plist :array-type 'list
                                    :null-object nil :false-object nil))))
                    (setq st (bebop--hook-fold st ev))))
                ;; The temp buffer holds raw bytes, so positions are bytes.
                (setq offset (+ offset (1- end)))))))
        (setq st (plist-put st :offset offset))
        (puthash name st bebop--hook-states)
        st))))

(defun bebop--hook-effective (st)
  "Roll ST's children up into the session's own state.
A session is as urgent as its most urgent sub-agent, so a blocked
sub-agent blocks the session and is named in the message. Returns a
plist :state :kind :msg :children."
  (let ((state (plist-get st :state))
        (kind (plist-get st :kind))
        (msg (plist-get st :msg)))
    (dolist (c (plist-get st :children))
      (let ((rec (cdr c)))
        (when (> (bebop--hook-rank (plist-get rec :state))
                 (bebop--hook-rank state))
          (setq state (plist-get rec :state)
                kind (plist-get rec :kind)
                msg (and (plist-get rec :msg)
                         (format "%s: %s" (or (plist-get rec :title) (car c))
                                 (plist-get rec :msg)))))))
    (list :state state :kind kind :msg msg :children (plist-get st :children))))

(defun bebop--pane-interrupted-p (pane-id)
  "Non-nil if PANE-ID shows an interrupted turn and no running one."
  (when-let ((raw (bebop--tmux-capture-pane pane-id)))
    (let* ((clean (bebop--strip-ansi raw))
           (tail (string-join
                  (last (seq-remove (lambda (l) (string-empty-p (string-trim l)))
                                    (split-string clean "\n"))
                        8)
                  "\n")))
      (and (cl-some (lambda (re) (string-match-p re tail))
                    bebop-interrupt-indicators)
           (not (string-match-p bebop-working-footer tail))))))

(defun bebop--status-detail (name pane-id)
  "Return the status detail of session NAME: hook events first, the pane second.
The pane alone decides that a window is gone, and it decides everything
for a session without hook events."
  (let ((pane (bebop--infer-status-detail pane-id))
        (st (bebop--hook-state name)))
    (if (or (eq (plist-get pane :status) 'gone)
            (null (plist-get st :state)))
        pane
      ;; An interrupt leaves the events at `working'; the pane says
      ;; otherwise. Fold that into the state so it holds until the next
      ;; event.
      (when (and (equal (plist-get st :state) "working")
                 (bebop--pane-interrupted-p pane-id))
        (setq st (plist-put st :state "idle"))
        (puthash name st bebop--hook-states))
      (bebop--hook-detail st))))

(defun bebop--hook-detail (st)
  "Return the status detail that hook state ST alone gives."
  (let ((eff (bebop--hook-effective st)))
    (list :status (pcase (plist-get eff :state)
                    ("working" 'active)
                    ((or "blocked" "error") 'blocked)
                    (_ 'waiting))
          :reason 'hook
          :hook eff)))

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
  (run-hook-with-args 'bebop-session-end-functions name)
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
  '("n: new  k: kill  RET: select  g: refresh")
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
                            (get-text-property (point) 'bebop-on-deck-name)))
            (saved-line (line-number-at-pos)))
        (erase-buffer)
        ;; Inherit each section's fold state from the previous render's
        ;; tree (magit's oldroot mechanism). With the default
        ;; magit-section-preserve-visibility=t, only the toggle cache
        ;; preserves folds — any state not in the cache resets to
        ;; expanded. Binding it nil makes preservation structural: the
        ;; on-screen state carries over exactly; hide arguments apply
        ;; only to sections that did not exist before.
        (let ((magit-section-preserve-visibility nil))
          (funcall bebop--render-body-function))
        ;; Materialize visual state from the hidden slots. Insertion only
        ;; SETS the slots — the invisibility overlays are created solely
        ;; by magit-section-hide, which magit's own refresh reaches via a
        ;; post-render magit-section-show/hide walk. Without this pass,
        ;; slot-hidden sections render expanded and the first TAB merely
        ;; syncs the slot (the double-TAB symptom), and hide defaults
        ;; like the collapsed Ungrouped never took effect visually.
        (dolist (child (oref magit-root-section children))
          (if (oref child hidden)
              (magit-section-hide child)
            (magit-section-show child)))
        ;; Footer. font-lock is active in magit-section buffers and strips
        ;; plain face properties — set font-lock-face as well.
        (insert "\n")
        (when (bound-and-true-p bebop-use-docker)
          (insert (propertize "Docker sandbox: ON  (! to toggle)\n"
                              'face '(:inherit warning :weight bold)
                              'font-lock-face '(:inherit warning :weight bold))))
        (dolist (line bebop-dashboard-footer-lines)
          (insert (propertize (concat line "\n")
                              'face '(:inherit shadow :height 0.75)
                              'font-lock-face '(:inherit shadow :height 0.75))))
        ;; Restore point: the same entry by name if it still exists,
        ;; otherwise the same line number — a killed or exiled entry
        ;; must not bounce point to the top of the buffer.
        (let ((found nil))
          (when saved-name
            (goto-char (point-min))
            (while (and (not found) (< (point) (point-max)))
              (if (or (equal (get-text-property (point) 'bebop-session-name)
                             saved-name)
                      (equal (get-text-property (point) 'bebop-on-deck-name)
                             saved-name))
                  (setq found t)
                (forward-line 1))))
          (unless found
            (goto-char (point-min))
            (forward-line (1- saved-line))))))))

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
         (status-face
          (cond
           ((memq status '(blocked waiting)) 'bebop-dot-waiting-face)
           ((memq status '(unknown degraded)) 'bebop-dot-degraded-face)
           (t 'bebop-dot-active-face)))
         ;; Attention outranks decay — the same ordering the setlist
         ;; gutter uses. Staleness may only gray a status you have
         ;; already acked, or this body shadows the one transition it
         ;; was meant to show you. bebop-set owns the ack registry and
         ;; this body is the bebop-set-unloaded fallback, so a missing
         ;; predicate reads honestly as "no acks known, decay freely".
         (dot-face (if (and (fboundp 'bebop-set--unseen-status-p)
                            (bebop-set--unseen-status-p name status))
                       status-face
                     (if (bebop--dot-stale-p name) 'shadow status-face)))
         (start (point)))
    ;; Insert pieces separately so the dot keeps its color face.
    ;; Propertizing the whole formatted line at once would overwrite
    ;; the dot's face with line-face.
    (insert (propertize "●" 'face dot-face))
    (insert " ")
    (insert (propertize name 'face line-face))
    (insert "\n")
    ;; bebop-session-name must cover the whole line for navigation to work
    (add-text-properties start (point) (list 'bebop-session-name name)))
  (bebop--insert-subagent-rows name "  "))

(defcustom bebop-subagent-rows-max 5
  "Most sub-agent rows shown under one session; the rest are counted."
  :type 'integer
  :group 'bebop)

(defun bebop--subagents (name)
  "Return session NAME's live sub-agents: an alist of AGENT-ID → record."
  (plist-get (plist-get (alist-get name bebop--last-status-details
                                   nil nil #'equal)
                        :hook)
             :children))

(defun bebop--subagent-signature (detail)
  "What a sub-agent row of status DETAIL shows, for change detection."
  (mapcar (lambda (c)
            (let ((r (cdr c)))
              (list (car c) (plist-get r :state) (plist-get r :title)
                    (plist-get r :msg))))
          (plist-get (plist-get detail :hook) :children)))

(defun bebop--subagent-prop (string face)
  "STRING with FACE as both `face' and `font-lock-face'."
  (propertize string 'face face 'font-lock-face face))

(defun bebop--insert-subagent-rows (name indent &optional children)
  "Insert a row for each live sub-agent of session NAME, after INDENT.
CHILDREN overrides the local hook state; the remote dashboard passes
the far session's."
  (let* ((children (or children (bebop--subagents name)))
         (shown (seq-take children bebop-subagent-rows-max))
         (more (- (length children) (length shown))))
    (cl-loop
     for (id . rec) in shown
     for i from 1
     do (let* ((last (and (= i (length shown)) (= more 0)))
               (blocked (equal (plist-get rec :state) "blocked"))
               (title (truncate-string-to-width
                       (or (plist-get rec :title) id) 40 nil nil "…"))
               (type (plist-get rec :type))
               (start (point)))
          (insert indent)
          (insert (bebop--subagent-prop (if last "└╴" "├╴") 'shadow))
          (insert (if blocked
                      (bebop--subagent-prop "●" 'bebop-dot-waiting-face)
                    (bebop--subagent-prop "◦" 'bebop-dot-active-face)))
          (insert " ")
          (insert (bebop--subagent-prop title '(:inherit shadow :slant italic)))
          (when (and type (not (equal type title)))
            (insert (bebop--subagent-prop (concat "  " type)
                                          '(:inherit shadow :height 0.85))))
          (when (and blocked (plist-get rec :msg))
            (insert "  ")
            (insert (bebop--subagent-prop
                     (truncate-string-to-width (plist-get rec :msg) 60 nil nil "…")
                     'bebop-dot-waiting-face)))
          (insert "\n")
          (add-text-properties start (point)
                               (list 'bebop-subagent-of name))))
    (when (> more 0)
      (let ((start (point)))
        (insert indent
                (bebop--subagent-prop (format "└╴+%d more" more) 'shadow)
                "\n")
        (add-text-properties start (point) (list 'bebop-subagent-of name))))))

(defun bebop--session-at-point ()
  "Return the pair name at point, or nil."
  (get-text-property (point) 'bebop-session-name))

(defun bebop--nav-stop-p ()
  "Non-nil on a line the arrow keys stop at.
Session lines, plus the derived sets' headings and MR rows: those
carry no session name, and arrows that skipped them left the region
reachable only by C-n."
  (or (get-text-property (point) 'bebop-session-name)
      (get-text-property (point) 'bebop-mr-item)
      (get-text-property (point) 'bebop-mr-heading)))

(defun bebop--next-session ()
  "Move point to the next pair line."
  (interactive)
  (let ((start (point)))
    (when (bebop--nav-stop-p)
      (forward-line 1))
    (while (and (< (point) (point-max))
                (null (bebop--nav-stop-p)))
      (forward-line 1))
    (unless (bebop--nav-stop-p)
      (goto-char start))))

(defun bebop--prev-session ()
  "Move point to the previous pair line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (> (point) (point-min))
                (null (bebop--nav-stop-p)))
      (forward-line -1))
    (unless (bebop--nav-stop-p)
      (goto-char start))))

(declare-function bebop-derived-act "bebop-set")
(declare-function bebop-derived--item-at-point "bebop-set")

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
      (if-let ((item (and (fboundp 'bebop-derived--item-at-point)
                          (bebop-derived--item-at-point))))
          (bebop-derived-act item)
        (message "No pair at point")))))

(defvar bebop-dashboard-mode-map (make-sparse-keymap)
  "Keymap for `bebop-dashboard-mode'.")

;; Bound at top level, not inside the defvar: a reload never re-runs a
;; defvar, so a key added there would never reach a running Emacs (the
;; remote dashboard's map stayed stale for weeks that way).
(dolist (b '(("n"      . bebop-new-session)
             ("RET"    . bebop-activate-at-point)
             ("<down>" . bebop--next-session)
             ("<up>"   . bebop--prev-session)
             ("j"      . bebop--next-session)
             ("p"      . bebop--prev-session)
             ("g"      . bebop-refresh)
             ("D"      . bebop-debug-pane)
             ("G"      . bebop-pulse-run)
             ("h"      . bebop-dashboard-next-tab)))
  (define-key bebop-dashboard-mode-map (kbd (car b)) (cdr b)))

;; Explicit, unconditional: `define-derived-mode' only sets the keymap
;; parent when the map has none, and `defvar' does not reinitialize on
;; hot-reload — so a live session that predates the magit-section
;; derivation would keep the stale parent and TAB would never reach
;; `magit-section-toggle'.
(set-keymap-parent bebop-dashboard-mode-map magit-section-mode-map)

;; q is neutralized, not merely unbound: both parent keymaps
;; (magit-section-mode-map and special-mode-map) bind it to
;; quit-window, and burying the Conductor's dashboard pane is never
;; intentional. Lives outside the defvar so hot-reloads apply it.
(define-key bebop-dashboard-mode-map (kbd "q") #'ignore)

(defun bebop--header-propertize (text &optional color)
  "TEXT styled as the dashboard header: bold, large, COLOR or the header blue.
Sized from the default face, so it ignores any text-scale."
  (let* ((resolved (and (fboundp 'bebop--resolve-font) (bebop--resolve-font)))
         (base-h (let ((dh (face-attribute 'default :height nil t)))
                   (if (integerp dh) dh 160)))
         (abs-h (round (* bebop-header-height base-h))))
    (propertize text
                'face `(:height ,abs-h :weight bold
                        :foreground ,(or color bebop-header-color)
                        ,@(when resolved (list :family resolved))))))

(defvar bebop-dashboard-tabs-function nil
  "Function returning the dashboard tabs as (LABEL . BUFFER), or nil.
When it returns tabs, the header reads \"Bebop: LABEL LABEL ...\", the
current buffer's label in the header blue. bebop-remote sets it to the
host tabs: this machine's *bebop* and the host's *bebop-remote*.")

(defun bebop-dashboard-switch-tab (buffer)
  "Show dashboard BUFFER in the selected window, keeping its dedication."
  (let ((dedicated (window-dedicated-p)))
    (set-window-dedicated-p nil nil)
    (switch-to-buffer buffer nil t)
    (set-window-dedicated-p nil dedicated)))

(defun bebop-dashboard-next-tab ()
  "Show the next dashboard tab in this window."
  (interactive)
  (let* ((tabs (and bebop-dashboard-tabs-function
                    (funcall bebop-dashboard-tabs-function)))
         (bufs (mapcar #'cdr tabs))
         (here (seq-position bufs (current-buffer))))
    (if (cdr bufs)
        (bebop-dashboard-switch-tab
         (nth (mod (1+ (or here -1)) (length bufs)) bufs))
      (message "Bebop: no other dashboard tab"))))

(defun bebop--dashboard-header ()
  "The header text: \"Bebop\", then the tabs inline when there are any.
A tab label is clickable; =h= cycles the tabs from the keyboard."
  (let ((tabs (and bebop-dashboard-tabs-function
                   (funcall bebop-dashboard-tabs-function))))
    (concat
     (bebop--dashboard-title tabs)
     (when (fboundp 'bebop--pulse-health)
       (bebop--pulse-segment (bebop--pulse-health) #'bebop-pulse-run)))))

(defun bebop--dashboard-title (tabs)
  "The header title: \"Bebop\", then TABS inline when there are two or more."
  (if (null (cdr tabs))
      (bebop--header-propertize (concat " " bebop-header-text))
    (concat
     (bebop--header-propertize (concat " " bebop-header-text ":"))
     (mapconcat
      (lambda (tab)
        (let ((buf (cdr tab))
              (map (make-sparse-keymap)))
          (define-key map [header-line mouse-1]
                      (lambda (ev)
                        (interactive "e")
                        (with-selected-window (posn-window (event-start ev))
                          (bebop-dashboard-switch-tab buf))))
          (propertize
           (bebop--header-propertize
            (concat " " (car tab))
            (unless (eq buf (current-buffer))
              (face-attribute 'shadow :foreground nil t)))
           ;; Hover as the eshell tabs do: the text lightens, no
           ;; background or box (`tab-line-highlight' in emacs.org).
           'mouse-face (list :foreground
                             (face-attribute (if (facep 'tab-line-highlight)
                                                 'tab-line-highlight
                                               'default)
                                             :foreground nil t))
           'help-echo (format "Show %s" (car tab))
           'local-map map)))
      tabs ""))))

(defun bebop--dashboard-look ()
  "Give the current buffer the dashboard's look.
Shared with the remote dashboard (`bebop-remote-mode'), which shows
the host's tree and must look the same."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Flush the gutter against the window edge: no left fringe. The
  ;; fringe is where magit draws its fold-caret bitmaps, and its textual
  ;; style marks only folded headings, so bebop marks both states
  ;; itself (▾ open, ▸ folded: `bebop-set--mark-folds').
  (setq-local left-fringe-width 0)
  (setq-local magit-section-visibility-indicators nil)
  (setq-local line-spacing -6)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1))
  (setq mode-line-format nil)
  (face-remap-add-relative 'header-line
                           :background (face-attribute 'default :background nil t)
                           :box nil
                           :underline nil))

(define-derived-mode bebop-dashboard-mode magit-section-mode "BebopDash"
  "Major mode for the Bebop multi-agent dashboard.
Derived from `magit-section-mode' for section folding (TAB) with
fold state preserved across poll re-renders via the visibility cache.
\\{bebop-dashboard-mode-map}"
  (setq-local magit-section-cache-visibility t)
  (bebop--dashboard-look)
  (setq header-line-format '((:eval (bebop--dashboard-header))))
  (add-hook 'kill-buffer-hook #'bebop--stop-poll nil t))

(defun bebop--notify-waiting (name)
  "Alert the user that agent pair NAME has finished and is waiting for input."
  (bebop-alert)
  (message "Bebop: %s is waiting for your input" name))

(defun bebop--apply-detail (name info detail)
  "Record DETAIL as the status of session NAME, whose registry plist is INFO.
Return non-nil when the row must be drawn again."
  (let ((changed nil)
        (new-status (plist-get detail :status))
        (old-status (plist-get info :status)))
    ;; Sub-agents come and go inside one status: redraw for them too.
    (unless (equal (bebop--subagent-signature detail)
                   (bebop--subagent-signature
                    (alist-get name bebop--last-status-details
                               nil nil #'equal)))
      (setq changed t))
    (setf (alist-get name bebop--last-status-details nil nil #'equal)
          detail)
    (unless (eq new-status 'gone)
      (unless (eq new-status old-status)
        (plist-put info :status new-status)
        (setq changed t)
        (when (memq new-status '(waiting blocked))
          (bebop--notify-waiting name))
        ;; Activity means the agent crossed between working and
        ;; waiting. A nil old-status is discovery (first poll
        ;; after restart), and degraded/unknown edges are infra
        ;; noise — neither says the agent did anything.
        (when (and (memq old-status '(active waiting blocked))
                   (memq new-status '(active waiting blocked)))
          (run-hook-with-args 'bebop-session-activity-functions
                              name 'transition)))
      (when (and (eq new-status 'unknown)
                 (not (eq old-status 'unknown)))
        (message "Bebop: %s status uncertain (%s)"
                 name (plist-get detail :reason))))
    changed))

(defun bebop--poll-all ()
  "Sweep all known pairs and update their status. Re-render if anything changed."
  (bebop--events-watch-ensure)
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
            (let* ((detail (bebop--status-detail name pane-id))
                   (old-status (plist-get info :status)))
              (bebop--transcript-track name (gethash name bebop--hook-states))
              (when (bebop--apply-detail name info detail)
                (setq changed t))
              (if (eq (plist-get detail :status) 'gone)
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
                (plist-put info :capture-failures 0))))))
      ;; Handle orphans after iteration completes to avoid mutating the list mid-loop
      (dolist (name orphans)
        (bebop--handle-orphan name))
      (bebop--transcripts-prune)
      ;; Render on change, and on an idle cadence so the age column
      ;; stays honest — a HUD showing stale timestamps is worse than
      ;; no timestamps.
      (when (or changed
                (>= (- (float-time) bebop--last-idle-render)
                    bebop-idle-render-interval))
        (setq bebop--last-idle-render (float-time))
        (bebop--render)))))

(defvar bebop--events-dir-watch nil
  "The file-notify descriptor watching `bebop-events-dir'.")

(defvar bebop--events-watches (make-hash-table :test #'equal)
  "Event file → its file-notify descriptor.")

(defvar bebop--push-render-timer nil
  "Pending render after pushed events: one draw for a burst.")

(defun bebop--events-watch-file (file)
  "Watch event FILE unless a live watch already covers it."
  (let ((desc (gethash file bebop--events-watches)))
    (unless (and desc (file-notify-valid-p desc))
      (condition-case err
          (puthash file (file-notify-add-watch file '(change)
                                               #'bebop--events-notify)
                   bebop--events-watches)
        (error (message "Bebop: cannot watch %s: %s"
                        file (error-message-string err)))))))

(defun bebop--events-watch-ensure ()
  "Watch the events folder and every event file in it.
Cheap when all the watches run: the sweep calls it every time."
  (when (fboundp 'file-notify-add-watch)
    (make-directory bebop-events-dir t)
    (unless (and bebop--events-dir-watch
                 (file-notify-valid-p bebop--events-dir-watch))
      (setq bebop--events-dir-watch
            (file-notify-add-watch bebop-events-dir '(change)
                                   #'bebop--events-notify)))
    (dolist (file (directory-files bebop-events-dir t "\\.jsonl\\'"))
      (bebop--events-watch-file file))))

(defun bebop--events-watch-stop ()
  "Remove every event watch."
  (when bebop--events-dir-watch
    (file-notify-rm-watch bebop--events-dir-watch)
    (setq bebop--events-dir-watch nil))
  (maphash (lambda (_ desc) (file-notify-rm-watch desc)) bebop--events-watches)
  (clrhash bebop--events-watches)
  (bebop--transcripts-stop))

(defun bebop--events-notify (event)
  "Handle file-notify EVENT from the events folder or an event file."
  (condition-case-unless-debug err
      (let ((action (nth 1 event))
            (file (nth 2 event)))
        (when (and (stringp file) (string-suffix-p ".jsonl" file))
          (pcase action
            ('created (bebop--events-watch-file file)
                      (bebop--hook-push file))
            ('changed (bebop--hook-push file))
            ((or 'deleted 'renamed)
             (when-let ((desc (gethash file bebop--events-watches)))
               (file-notify-rm-watch desc)
               (remhash file bebop--events-watches))))))
    (error (message "Bebop: event watch: %s" (error-message-string err)))))

(defun bebop--session-for-events-file (file)
  "Return the live session (NAME . INFO) whose event file is FILE."
  (seq-find (lambda (pair)
              (string= (bebop--events-file (car pair)) file))
            bebop--live-sessions))

(defun bebop--hook-push (file)
  "Fold the new events in FILE into its session's status."
  (when-let* ((pair (bebop--session-for-events-file file))
              (name (car pair))
              (info (cdr pair)))
    (unless (plist-get info :pane-id)
      (when-let ((id (bebop--tmux-pane-id-for (bebop--tmux-window-name name))))
        (plist-put info :pane-id id)))
    (let ((st (bebop--hook-state name)))
      (bebop--transcript-track name st)
      (when (and st
                 (if (plist-get st :state)
                     (bebop--apply-detail name info (bebop--hook-detail st))
                   (and (plist-get info :pane-id)
                        (bebop--apply-detail
                         name info
                         (bebop--status-detail name (plist-get info :pane-id))))))
        (bebop--push-render)))))

(defun bebop--push-render ()
  "Draw the dashboard once, 0.2 s after the last pushed change."
  (when (timerp bebop--push-render-timer)
    (cancel-timer bebop--push-render-timer))
  (setq bebop--push-render-timer
        (run-at-time 0.2 nil (lambda ()
                               (setq bebop--push-render-timer nil)
                               (bebop--render)))))

(require 'parse-time)

(defconst bebop--interrupt-marker "[Request interrupted by user"
  "Text that starts the transcript message Claude writes on an Esc interrupt.")

(defvar bebop--transcripts (make-hash-table :test #'equal)
  "Session name → plist :file :offset :watch for its watched transcript.")

(defun bebop--transcript-watched-p (name)
  "Non-nil when session NAME's transcript has a live watch."
  (when-let ((watch (plist-get (gethash name bebop--transcripts) :watch)))
    (file-notify-valid-p watch)))

(defun bebop--transcript-unwatch (name)
  "Stop watching session NAME's transcript."
  (when-let ((watch (plist-get (gethash name bebop--transcripts) :watch)))
    (file-notify-rm-watch watch))
  (remhash name bebop--transcripts))

(defun bebop--transcript-track (name st)
  "Watch the transcript that hook state ST names for session NAME.
A watch on another file goes, and a watch that stopped comes back. A new
watch starts at the end of the file: only new interrupts matter."
  (let ((file (plist-get st :transcript))
        (tr (gethash name bebop--transcripts)))
    (cond
     ((or (null file) (not (fboundp 'file-notify-add-watch))
          (not (file-exists-p file)))
      (when tr (bebop--transcript-unwatch name)))
     ((and tr (equal (plist-get tr :file) file)
           (bebop--transcript-watched-p name)))
     (t
      (when tr (bebop--transcript-unwatch name))
      (condition-case err
          (puthash name
                   (list :file file
                         :offset (file-attribute-size (file-attributes file))
                         :watch (file-notify-add-watch
                                 file '(change) #'bebop--transcript-notify))
                   bebop--transcripts)
        (error (message "Bebop: cannot watch %s: %s"
                        file (error-message-string err))))))))

(defun bebop--transcripts-stop ()
  "Stop watching every transcript."
  (dolist (name (hash-table-keys bebop--transcripts))
    (bebop--transcript-unwatch name)))

(defun bebop--transcripts-prune ()
  "Stop watching the transcripts of sessions that are no longer live."
  (dolist (name (hash-table-keys bebop--transcripts))
    (unless (assoc name bebop--live-sessions)
      (bebop--transcript-unwatch name))))

(defun bebop--transcript-interrupt-time (file from to)
  "Time of the last interrupt marker in FILE's bytes FROM to TO, or nil.
Only whole lines count; the caller passes TO at a line end."
  (with-temp-buffer
    (insert-file-contents-literally file nil from to)
    (let ((last nil))
      (goto-char (point-min))
      (while (search-forward bebop--interrupt-marker nil t)
        (when-let* ((line (decode-coding-string
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           'utf-8))
                    (rec (ignore-errors
                           (json-parse-string line :object-type 'plist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil)))
                    ((equal (plist-get rec :type) "user"))
                    (content (plist-get (plist-get rec :message) :content))
                    ((cl-some (lambda (c)
                                (and (equal (plist-get c :type) "text")
                                     (string-prefix-p bebop--interrupt-marker
                                                      (or (plist-get c :text) ""))))
                              (if (listp content) content nil)))
                    (stamp (plist-get rec :timestamp)))
          (setq last (float-time (parse-iso8601-time-string stamp t))))
        (forward-line 1))
      last)))

(defun bebop--transcript-notify (event)
  "Fold an Esc interrupt from the transcript that file-notify EVENT names."
  (condition-case-unless-debug err
      (let ((file (nth 2 event))
            (name nil))
        (maphash (lambda (n tr) (when (equal (plist-get tr :file) file)
                                  (setq name n)))
                 bebop--transcripts)
        (when (and name (eq (nth 1 event) 'changed))
          (let* ((tr (gethash name bebop--transcripts))
                 (from (plist-get tr :offset))
                 (size (file-attribute-size (file-attributes file))))
            (cond
             ((null size))
             ((< size from) (plist-put tr :offset size))
             ((> size from)
              ;; Stop at the last newline: a line still being written waits.
              (let ((to (with-temp-buffer
                          (insert-file-contents-literally file nil from size)
                          (goto-char (point-max))
                          (and (search-backward "\n" nil t)
                               (+ from (point))))))
                (when to
                  (plist-put tr :offset to)
                  (when-let* ((at (bebop--transcript-interrupt-time file from to))
                              (st (gethash name bebop--hook-states))
                              ((member (plist-get st :state) '("working" "blocked")))
                              ((> at (or (plist-get st :at) 0)))
                              (info (cdr (assoc name bebop--live-sessions))))
                    (setq st (plist-put st :state "idle"))
                    (setq st (plist-put st :kind nil))
                    (setq st (plist-put st :msg nil))
                    (puthash name st bebop--hook-states)
                    (when (bebop--apply-detail name info (bebop--hook-detail st))
                      (bebop--push-render))))))))))
    (error (message "Bebop: transcript watch: %s" (error-message-string err)))))

(defun bebop--interrupt-check ()
  "Fold an Esc interrupt into each quiet working session that shows one."
  (let ((changed nil)
        (quiet-since (- (float-time) bebop-interrupt-check-interval)))
    (dolist (pair bebop--live-sessions)
      (let* ((name (car pair))
             (info (cdr pair))
             (pane-id (plist-get info :pane-id))
             (st (gethash name bebop--hook-states)))
        (when (and pane-id
                   (not (bebop--transcript-watched-p name))
                   (equal (plist-get st :state) "working")
                   (< (or (plist-get st :at) 0) quiet-since)
                   (bebop--pane-interrupted-p pane-id))
          (setq st (plist-put st :state "idle"))
          (puthash name st bebop--hook-states)
          (when (bebop--apply-detail name info (bebop--hook-detail st))
            (setq changed t)))))
    (when changed
      (bebop--render))))

(defun bebop-refresh ()
  "Manually trigger a full resync: rediscover pairs, poll status, re-render.
Also restarts the poll timer if it stopped."
  (interactive)
  (bebop--discover-existing-sessions)
  (bebop--poll-all)
  (bebop--render)
  (bebop--start-poll))

(defun bebop-start ()
  "Ensure the tmux session, discover running agents, and start polling."
  (interactive)
  (bebop--ensure-tmux-session)
  (bebop--discover-existing-sessions)
  (bebop--start-poll))

(add-hook 'emacs-startup-hook #'bebop-start)

(defun bebop--start-poll ()
  "Start the event watches, the sweep, the interrupt check and pulse.
The first three restart; a running pulse loop keeps its schedule."
  (bebop--events-watch-ensure)
  (bebop--loop-start 'poll bebop-sweep-interval #'bebop--poll-all)
  (bebop--loop-start 'interrupt bebop-interrupt-check-interval
                     #'bebop--interrupt-check)
  (bebop--pulse-start))

(defun bebop--stop-poll ()
  "Stop the event watches, the sweep, the interrupt check and pulse."
  (bebop--events-watch-stop)
  (bebop--loop-stop 'poll)
  (bebop--loop-stop 'interrupt)
  (bebop--pulse-stop))

(when (timerp bebop--poll-timer)
  (cancel-timer bebop--poll-timer)
  (setq bebop--poll-timer nil)
  (bebop--start-poll))

(when (and (bebop--loop-running-p 'poll)
           (not (bebop--loop-running-p 'interrupt)))
  (bebop--start-poll))

(defcustom bebop-pulse-interval 600
  "Seconds between pulse runs."
  :type 'integer
  :group 'bebop)

(defcustom bebop-pulse-timeout 300
  "Seconds after which a pulse run counts as hung and is killed."
  :type 'integer
  :group 'bebop)

(defcustom bebop-pulse-log (expand-file-name "~/.emacs.d-local/pulse.log")
  "File that pulse output is appended to."
  :type 'file
  :group 'bebop)

(defvar bebop--pulse nil
  "Pulse run state: a plist :proc :rerun :started :ended :exit and
:sources, an alist of SOURCE → plist :ok :at :ok-at :error (:at is the
last result, :ok-at the last success).")

(defun bebop--pulse-script ()
  "The pulse script."
  (expand-file-name "bin/bebop-pulse.py" user-emacs-directory))

(defun bebop-pulse-run ()
  "Start a pulse run in the background, unless one runs already.
A request during a run asks for one more run after it."
  (interactive)
  (let ((proc (plist-get bebop--pulse :proc)))
    (when (and (process-live-p proc)
               (> (- (float-time) (plist-get bebop--pulse :started))
                  bebop-pulse-timeout))
      (process-put proc :timed-out t)
      (delete-process proc))
    (cond
     ((process-live-p proc)
      (setq bebop--pulse (plist-put bebop--pulse :rerun t)))
     ((null bebop--live-sessions))
     (t
      (setq bebop--pulse
            (plist-put
             (plist-put
              (plist-put bebop--pulse :rerun nil)
              :started (float-time))
             :proc (make-process
                    :name "bebop-pulse"
                    :buffer (generate-new-buffer " *bebop-pulse*")
                    :command (list (or (executable-find "python3")
                                       "/usr/bin/python3")
                                   (bebop--pulse-script))
                    :connection-type 'pipe
                    :noquery t
                    :sentinel #'bebop--pulse-sentinel)))
      (force-mode-line-update t)))))

(defun bebop--pulse-results (output)
  "Parse the PULSE result lines in OUTPUT: an alist SOURCE → plist."
  (let ((results nil)
        (start 0))
    (while (string-match "^PULSE \\(\\S-+\\) \\(ok\\|fail\\)\\(?: \\(.*\\)\\)?$"
                         output start)
      (setf (alist-get (match-string 1 output) results nil nil #'equal)
            (list :ok (equal (match-string 2 output) "ok")
                  :at (float-time)
                  :error (match-string 3 output)))
      (setq start (match-end 0)))
    results))

(defun bebop--pulse-sentinel (proc _event)
  "Record how pulse run PROC ended, log its output, and run again if asked."
  (unless (process-live-p proc)
    (let* ((buf (process-buffer proc))
           (output (if (buffer-live-p buf)
                       (with-current-buffer buf (buffer-string))
                     ""))
           (exit (process-exit-status proc))
           (results (bebop--pulse-results output))
           (sources (plist-get bebop--pulse :sources)))
      ;; A failure keeps the time of the source's last success.
      (dolist (r results)
        (let ((prev (alist-get (car r) sources nil nil #'equal)))
          (setf (alist-get (car r) sources nil nil #'equal)
                (plist-put (cdr r) :ok-at
                           (if (plist-get (cdr r) :ok)
                               (plist-get (cdr r) :at)
                             (plist-get prev :ok-at))))))
      ;; A run that died without a word: blame GitLab, the only source
      ;; that has always been there, with the last line it printed.
      (when (and (null results) (not (zerop exit)))
        (setf (alist-get "gitlab" sources nil nil #'equal)
              (list :ok nil :at (float-time)
                    :ok-at (plist-get (alist-get "gitlab" sources nil nil #'equal)
                                      :ok-at)
                    :error (if (process-get proc :timed-out)
                               (format "timed out after %d s"
                                       bebop-pulse-timeout)
                             (or (car (last (split-string output "
" t)))
                                 (format "exit %d" exit))))))
      (setq bebop--pulse
            (plist-put
             (plist-put
              (plist-put bebop--pulse :sources sources)
              :exit exit)
             :ended (float-time)))
      (ignore-errors
        (make-directory (file-name-directory bebop-pulse-log) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (format "%s pulse exit %d\n%s"
                                (format-time-string "%F %T") exit output)
                        nil bebop-pulse-log t 'silent)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (force-mode-line-update t)
      (when (plist-get bebop--pulse :rerun)
        (run-at-time 0 nil #'bebop-pulse-run)))))

(defun bebop--pulse-start ()
  "Start the pulse loop unless it runs; run once soon if pulse is stale."
  (unless (bebop--loop-running-p 'pulse)
    (bebop--loop-start 'pulse bebop-pulse-interval #'bebop-pulse-run)
    (when (> (- (float-time) (or (plist-get bebop--pulse :ended) 0))
             bebop-pulse-interval)
      ;; Give discovery a moment to find the live sessions.
      (run-at-time 5 nil #'bebop-pulse-run))))

(defun bebop--pulse-stop ()
  "Stop the pulse loop. A run in progress finishes."
  (bebop--loop-stop 'pulse))

(defconst bebop--pulse-labels '(("gitlab" . "GitLab") ("jira" . "Jira"))
  "Display names of pulse sources, in display order.")

(defun bebop--pulse-health ()
  "This host's pulse health: a plist :running :interval :sources.
Plain data, so that a remote client can read it from the snapshot."
  (list :running (and (process-live-p (plist-get bebop--pulse :proc)) t)
        :interval bebop-pulse-interval
        :sources (mapcar (lambda (s)
                           (cons (car s)
                                 (list :ok (plist-get (cdr s) :ok)
                                       :ok-at (or (plist-get (cdr s) :ok-at)
                                                  (and (plist-get (cdr s) :ok)
                                                       (plist-get (cdr s) :at)))
                                       :error (plist-get (cdr s) :error))))
                         (plist-get bebop--pulse :sources))))

(defun bebop--age-text (seconds)
  "SECONDS as a short age: <1m, 4m, 3h, 2d."
  (cond ((< seconds 60) "<1m")
        ((< seconds 3600) (format "%dm" (/ seconds 60)))
        ((< seconds 86400) (format "%dh" (/ seconds 3600)))
        (t (format "%dd" (/ seconds 86400)))))

(require 'color)

(defun bebop--faded (face)
  "FACE's foreground blended halfway into the default background."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (fg bg) (/ (+ fg bg) 2))
                            (color-name-to-rgb (face-attribute face :foreground nil t))
                            (color-name-to-rgb (face-attribute 'default :background nil t)))
                 '(2))))

(defun bebop--pulse-segment (health run)
  "The header's pulse dot for HEALTH; a click calls RUN.
Empty when HEALTH has no sources."
  (let ((sources (plist-get health :sources))
        (stale (* 2 (or (plist-get health :interval) bebop-pulse-interval)))
        (map (make-sparse-keymap))
        (bad nil)
        (lines nil))
    (define-key map [header-line mouse-1]
                (lambda (_ev) (interactive "e") (funcall run)))
    (dolist (label bebop--pulse-labels)
      (when-let ((src (alist-get (car label) sources nil nil #'equal)))
        (let* ((ok-at (plist-get src :ok-at))
               (age (and ok-at (round (- (float-time) ok-at)))))
          (when (or (not (plist-get src :ok)) (null age) (> age stale))
            (setq bad t))
          (push (format "%s: %s%s" (cdr label)
                        (if age (concat (bebop--age-text age) " ago") "never")
                        (if (plist-get src :error)
                            (concat ", failed: " (plist-get src :error))
                          ""))
                lines))))
    (if (null lines)
        ""
      (concat
       "  "
       (propertize "●"
                   'face (list :foreground (bebop--faded (if bad 'error 'success)))
                   'mouse-face 'highlight
                   'local-map map
                   'help-echo
                   (concat "Pulse"
                           (when (plist-get health :running) " (running)")
                           "\n" (string-join (nreverse lines) "\n")
                           "\nClick, or G, to run pulse now"))))))

(when (bebop--loop-running-p 'poll)
  (bebop--pulse-start))

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
