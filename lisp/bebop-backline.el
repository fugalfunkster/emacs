;;; bebop-backline.el --- Venue-owned work shells for long-running processes -*- lexical-binding: t; -*-

(require 'bebop-core)
(require 'bebop-dashboard)
(require 'bebop-session)

(defcustom bebop-backline-prefix "backline--"
  "tmux window-name prefix identifying backline (venue work shell) windows.
Must match the prefix skipped by `bebop--discover-existing-sessions'."
  :type 'string
  :group 'bebop)

(defcustom bebop-backline-roster-source nil
  "Function returning this machine's service roster, or nil for none.
Called with no arguments; returns a list of plists, one per service:

  :name        service name (string)
  :port        TCP port it listens on (integer, or nil)
  :group       coarse grouping, e.g. \"fe\" / \"be\" (string or nil)
  :start-cmd   shell command a session runs in its venue's backline
               window to serve this service, or nil when unknown
  :machine-cmd shell command the machine runs to restore this service
               to its default state, or nil when unknown
  :broken      non-nil for services known not to start

nil is the honest default: with no roster the fleet view still works,
naming live listeners `port-NNNN'. See `bebop-backline-roster-stack-sh'
for the shipped adapter."
  :type '(choice (const :tag "None (lsof discovery only)" nil) function)
  :group 'bebop)

(defconst bebop-backline-machine "machine"
  "Holder name for a service no session holds — the machine's own.
Services with this holder survive every session teardown.")

(defvar bebop--backlines nil
  "Alist of (SLUG . PLIST) intent metadata for backline windows.
PLIST keys: :dir, :last-command, :started-at.
Convenience only — live state is always derived from tmux and lsof.")

(defun bebop--backline-slug-for-dir (dir)
  "Return the backline slug for DIR: its directory basename."
  (file-name-nondirectory (directory-file-name (expand-file-name dir))))

(defun bebop--backline-window-name (slug)
  "Return the tmux window name for backline SLUG."
  (concat bebop-backline-prefix slug))

(defun bebop--backline-window-id (slug)
  "Return the tmux window id (e.g. \"@5\") for backline SLUG, or nil.
Resolved by exact name match in Lisp, then used as the target for all
window operations. tmux name targets resolve by prefix and fnmatch and
treat \".\" as a pane separator — window ids have none of those hazards,
so a backline operation can never mis-target another window."
  (let ((want (bebop--backline-window-name slug))
        (out (bebop--tmux-output "list-windows" "-t" bebop-tmux-session
                                 "-F" "#{window_id}\t#{window_name}")))
    (when out
      (catch 'found
        (dolist (line (split-string out "\n" t))
          (let ((parts (split-string line "\t")))
            (when (equal (cadr parts) want)
              (throw 'found (car parts)))))))))

(defun bebop--backline-slugs ()
  "Return slugs of all live backline windows, derived from tmux."
  (delq nil
        (mapcar (lambda (w)
                  (when (string-prefix-p bebop-backline-prefix w)
                    (substring w (length bebop-backline-prefix))))
                (bebop--tmux-window-list))))

(defun bebop--backline-live-p (slug)
  "Return non-nil if a backline window exists for SLUG."
  (and (bebop--backline-window-id slug) t))

(defun bebop--backline-pane-format (slug fmt)
  "Return tmux FMT expanded for backline SLUG's pane, or nil."
  (when-let ((id (bebop--backline-window-id slug)))
    (bebop--tmux-output "display-message" "-p" "-t" id fmt)))

(defun bebop--backline-pane-pid (slug)
  "Return the shell PID of backline SLUG's pane, or nil."
  (let ((out (bebop--backline-pane-format slug "#{pane_pid}")))
    (when (and out (string-match-p "^[0-9]+$" out))
      (string-to-number out))))

(defun bebop--backline-current-command (slug)
  "Return the foreground command name in backline SLUG's pane, or nil."
  (bebop--backline-pane-format slug "#{pane_current_command}"))

(defun bebop--backline-busy-p (slug)
  "Return non-nil if backline SLUG is running a foreground process."
  (let ((cmd (bebop--backline-current-command slug)))
    (and cmd
         (not (member cmd '("zsh" "bash" "sh" "fish" "-zsh" "-bash" "-sh"))))))

(defun bebop--listening-pairs ()
  "Return alist of (PID . PORT) for listening TCP sockets, via lsof.
Best-effort: returns nil if lsof is unavailable or reports nothing.

lsof -F output groups fields per process: each group begins with a
\"p\" line, and every following field line (\"f\", \"n\", ...) belongs
to that process until the next \"p\" line. current-pid therefore
persists across intervening field lines by design — do not reset it
on unrecognized prefixes."
  (let (pairs current-pid)
    (with-temp-buffer
      (call-process "lsof" nil '(t nil) nil
                    "-iTCP" "-sTCP:LISTEN" "-P" "-n" "-Fpn")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "p" line)
            (setq current-pid (string-to-number (substring line 1))))
           ((and current-pid
                 (string-prefix-p "n" line)
                 (string-match ":\\([0-9]+\\)$" line))
            (let ((pair (cons current-pid
                              (string-to-number (match-string 1 line)))))
              (unless (member pair pairs)
                (push pair pairs))))))
        (forward-line 1)))
    (nreverse pairs)))

(defun bebop--process-table ()
  "Return a hash of PID → (PPID . COMMAND) for every process, in one ps call.
Ancestry and command line both come out of it, so a caller resolving
sixty listeners spends one subprocess instead of several hundred."
  (let ((table (make-hash-table :test #'eql)))
    (with-temp-buffer
      (when (eq 0 (call-process "ps" nil '(t nil) nil "-eo" "pid=,ppid=,command="))
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "[ \t]*\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)$")
            (puthash (string-to-number (match-string 1))
                     (cons (string-to-number (match-string 2))
                           (match-string 3))
                     table))
          (forward-line 1))))
    table))

(defun bebop--process-ancestors (pid &optional procs)
  "Return PID's ancestor PIDs (nearest first), bounded against cycles.
PROCS is an optional PID → (PPID . COMMAND) table from
`bebop--process-table'. Without one, every step up the tree costs its
own ps call."
  (let (result (current pid) (guard 0))
    (while (and current (> current 1) (< guard 64))
      (setq guard (1+ guard))
      (let ((ppid (if procs
                      (car (gethash current procs))
                    (with-temp-buffer
                      (when (eq 0 (call-process "ps" nil '(t nil) nil
                                                "-o" "ppid=" "-p"
                                                (number-to-string current)))
                        (let ((s (string-trim (buffer-string))))
                          (when (string-match-p "^[0-9]+$" s)
                            (string-to-number s))))))))
        (setq current (and ppid (> ppid 1) ppid))
        (when current (push current result))))
    (nreverse result)))

(defun bebop--backline-panes ()
  "Return an alist of (SLUG . PANE-PID) for every live backline window.
One tmux call for all of them: a backline window has a single pane, so
its active pane's shell is the ancestor everything under it shares."
  (delq nil
        (mapcar
         (lambda (line)
           (let* ((parts (split-string line "\t"))
                  (name (car parts))
                  (pid (cadr parts)))
             (when (and name pid
                        (string-prefix-p bebop-backline-prefix name)
                        (string-match-p "\\`[0-9]+\\'" pid))
               (cons (substring name (length bebop-backline-prefix))
                     (string-to-number pid)))))
         (split-string (or (bebop--tmux-output
                            "list-windows" "-t" bebop-tmux-session
                            "-F" "#{window_name}\t#{pane_pid}")
                           "")
                       "\n" t))))

(defun bebop--backline-owning-slug (pid &optional panes procs)
  "Return the backline slug whose pane shell is PID or an ancestor of PID.
PANES and PROCS are optional precomputed lookups — see
`bebop--backline-panes' and `bebop--process-table'."
  (let ((lineage (cons pid (bebop--process-ancestors pid procs))))
    (car (seq-find (lambda (cell) (memq (cdr cell) lineage))
                   (or panes (bebop--backline-panes))))))

(defun bebop-backline-port-owner (port)
  "Return a plist describing the listener on PORT, or nil if PORT is free.
Keys: :pid, :slug (backline slug or nil if the process is external)."
  (when-let ((pid (car (rassq port (bebop--listening-pairs)))))
    (list :pid pid :slug (bebop--backline-owning-slug pid))))

(defun bebop--backline-ports (slug)
  "Return the list of TCP ports held by processes under backline SLUG.
Ancestor walks are cached per PID so a process listening on several
ports costs one ps walk, not one per port."
  (when-let ((pane-pid (bebop--backline-pane-pid slug)))
    (let ((lineages (make-hash-table :test #'eql))
          (procs (bebop--process-table))
          ports)
      (dolist (pair (bebop--listening-pairs))
        (let* ((pid (car pair))
               (lineage (or (gethash pid lineages)
                            (puthash pid
                                     (cons pid (bebop--process-ancestors
                                                pid procs))
                                     lineages))))
          (when (memq pane-pid lineage)
            (push (cdr pair) ports))))
      (nreverse ports))))

(defun bebop-backline-roster ()
  "Return this machine's service roster, or nil if it has none.
Calls `bebop-backline-roster-source'; see that variable for the shape
of a roster entry. Fails soft: a source that errors gets one message
and an empty roster, never a broken fleet view."
  (when (functionp bebop-backline-roster-source)
    (condition-case err
        (funcall bebop-backline-roster-source)
      (error
       (message "Backline roster: %s failed — %s"
                bebop-backline-roster-source (error-message-string err))
       nil))))

(defun bebop-backline-service (name)
  "Return the roster entry named NAME, or nil."
  (seq-find (lambda (s) (equal (plist-get s :name) name))
            (bebop-backline-roster)))

(defun bebop--pid-cwds (pids)
  "Return a hash of PID → working directory for PIDS, in one lsof call.
lsof exits non-zero when any of PIDS has already gone; the pids that
answered are still good, so the status is not checked."
  (let ((table (make-hash-table :test #'eql))
        current)
    (when pids
      (with-temp-buffer
        (call-process "lsof" nil '(t nil) nil
                      "-a" "-p" (mapconcat #'number-to-string pids ",")
                      "-d" "cwd" "-Fpn")
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-prefix-p "p" line)
              (setq current (string-to-number (substring line 1))))
             ((and current (string-prefix-p "n" line))
              (puthash current (substring line 1) table))))
          (forward-line 1))))
    table))

(defun bebop--pid-cwd (pid)
  "Return PID's working directory via lsof, or nil."
  (gethash pid (bebop--pid-cwds (list pid))))

(defun bebop--backline-venue-in-path (path)
  "Return the venue directory name PATH sits under, or nil.
PATH may be a directory or a whole command line: the venues root is
distinctive enough to match inside either."
  (when (and path (stringp path))
    (let ((root (file-name-as-directory (expand-file-name bebop-venues-dir))))
      (when (string-match (concat (regexp-quote root) "\\([^/ \n]+\\)") path)
        (match-string 1 path)))))

(defun bebop-backline--context (pids)
  "Return the lookups the holder rule needs for PIDS, gathered once.
Three subprocesses (tmux, ps, lsof) serve any number of pids."
  (list :panes (bebop--backline-panes)
        :processes (bebop--process-table)
        :cwds (bebop--pid-cwds pids)))

(defun bebop-backline-holder (pid &optional ctx)
  "Return the derived holder of PID: a venue slug, or `bebop-backline-machine'.
Never stored, always derived — see the =* Backline= design section.
CTX is an optional context from `bebop-backline--context'; without one
the lookups are made for this pid alone."
  (let* ((procs (or (plist-get ctx :processes) (bebop--process-table)))
         (cwds (plist-get ctx :cwds)))
    (or (bebop--backline-owning-slug pid (plist-get ctx :panes) procs)
        (bebop--backline-venue-in-path
         (if cwds (gethash pid cwds) (bebop--pid-cwd pid)))
        (bebop--backline-venue-in-path (cdr (gethash pid procs)))
        bebop-backline-machine)))

(defun bebop-backline-services ()
  "Return the machine's service fleet: the roster merged with live lsof state.
One plist per service, sorted by port:

  :name :port :group :start-cmd :machine-cmd :broken  — from the roster
  :rostered  non-nil when the row came from the roster
  :up        non-nil when something is listening on :port
  :pid       the listening process, or nil
  :holder    derived holder (see `bebop-backline-holder'), nil when down

Live listeners with no roster entry are named \"port-NNNN\": the fleet
view stays honest about processes bebop was never told about."
  (let* ((pairs (bebop--listening-pairs))
         (ctx (bebop-backline--context (mapcar #'car pairs)))
         claimed rows)
    (dolist (svc (bebop-backline-roster))
      (let* ((port (plist-get svc :port))
             (pid (and port (car (rassq port pairs)))))
        (when pid (push port claimed))
        (push (append (list :rostered t
                            :up (and pid t)
                            :pid pid
                            :holder (and pid (bebop-backline-holder pid ctx)))
                      svc)
              rows)))
    (dolist (pair pairs)
      (let ((port (cdr pair)))
        (unless (memq port claimed)
          (push port claimed)
          (push (list :name (format "port-%d" port)
                      :port port
                      :rostered nil
                      :up t
                      :pid (car pair)
                      :holder (bebop-backline-holder (car pair) ctx))
                rows))))
    (sort (nreverse rows)
          (lambda (a b)
            (let ((pa (plist-get a :port)) (pb (plist-get b :port)))
              (cond ((and pa pb (/= pa pb)) (< pa pb))
                    ((and pa pb) (string< (plist-get a :name)
                                          (plist-get b :name)))
                    (pa t)
                    (pb nil)
                    (t (string< (plist-get a :name) (plist-get b :name)))))))))

(defun bebop-backline--service-row (name)
  "Return the fleet row named NAME, or nil.
NAME may be a roster name or an unrostered \"port-NNNN\" row."
  (seq-find (lambda (r) (equal (plist-get r :name) name))
            (bebop-backline-services)))

(defun bebop-backline-service-holder (name)
  "Return the derived holder of service NAME, or nil if it is down."
  (plist-get (bebop-backline--service-row name) :holder))

(defun bebop-backline-venue-services (slug)
  "Return the fleet rows whose derived holder is venue SLUG."
  (seq-filter (lambda (r) (equal (plist-get r :holder) slug))
              (bebop-backline-services)))

(defun bebop--backline-register (slug &rest props)
  "Merge PROPS into the intent metadata for SLUG."
  (let ((cell (assoc slug bebop--backlines)))
    (unless cell
      (setq cell (cons slug nil))
      (push cell bebop--backlines))
    (let ((plist (cdr cell)))
      (while props
        (setq plist (plist-put plist (car props) (cadr props)))
        (setq props (cddr props)))
      (setcdr cell plist))))

(defun bebop-backline-ensure (dir)
  "Ensure a backline window exists for DIR. Return its slug.
Creates the window detached (no focus steal) with DIR as its cwd."
  (let* ((dir (expand-file-name dir))
         (slug (bebop--backline-slug-for-dir dir)))
    (unless (file-directory-p dir)
      (user-error "Backline: not a directory: %s" dir))
    (unless (bebop--tmux-session-exists-p)
      (bebop--tmux "new-session" "-d" "-s" bebop-tmux-session))
    (unless (bebop--backline-live-p slug)
      (bebop--tmux "new-window" "-d" "-a"
                   "-t" bebop-tmux-session
                   "-n" (bebop--backline-window-name slug)
                   "-c" dir))
    (bebop--backline-register slug :dir dir)
    slug))

(defun bebop-backline-run (dir command)
  "Run COMMAND in the backline for DIR, creating the window if needed.
Signals an error if the backline is already running a foreground
process — interrupt or take over explicitly first. Returns the slug."
  (let ((slug (bebop-backline-ensure dir)))
    (when (bebop--backline-busy-p slug)
      (user-error "Backline %s is busy (%s) — bebop-backline-interrupt it first"
                  slug (bebop--backline-current-command slug)))
    (bebop--tmux "send-keys" "-t" (bebop--backline-window-id slug)
                 command "Enter")
    (bebop--backline-register slug
                              :last-command command
                              :started-at (format-time-string "%Y-%m-%d %H:%M"))
    slug))

(defun bebop-backline-interrupt (slug)
  "Send C-c to backline SLUG's foreground process."
  (interactive
   (list (completing-read "Interrupt backline: "
                          (or (bebop--backline-slugs)
                              (user-error "No backline windows exist"))
                          nil t)))
  (let ((id (bebop--backline-window-id slug)))
    (unless id
      (user-error "No backline window for %s" slug))
    (bebop--tmux "send-keys" "-t" id "C-c"))
  (message "Backline %s: interrupted" slug))

(defun bebop-backline-kill (slug)
  "Kill backline SLUG's tmux window and drop its metadata."
  (interactive
   (list (completing-read "Kill backline: "
                          (or (bebop--backline-slugs)
                              (user-error "No backline windows exist"))
                          nil t)))
  (when-let ((id (bebop--backline-window-id slug)))
    (ignore-errors
      (bebop--tmux "kill-window" "-t" id)))
  (setq bebop--backlines (assoc-delete-all slug bebop--backlines))
  (message "Backline %s: killed" slug))

(defun bebop-backline-status (&optional slug)
  "Return a status plist for backline SLUG, or a list for all backlines.
Keys: :slug :dir :busy :command :ports :last-command :started-at.

Interactively, displays `bebop-backline-fleet' instead — a return value
is no use at the keyboard."
  (interactive)
  (if (called-interactively-p 'interactive)
      (bebop-backline-fleet)
    (if slug
        (let ((meta (cdr (assoc slug bebop--backlines))))
          (list :slug slug
                :dir (plist-get meta :dir)
                :busy (and (bebop--backline-busy-p slug) t)
                :command (bebop--backline-current-command slug)
                :ports (bebop--backline-ports slug)
                :last-command (plist-get meta :last-command)
                :started-at (plist-get meta :started-at)))
      (mapcar #'bebop-backline-status (bebop--backline-slugs)))))

(defcustom bebop-backline-fleet-ephemeral-floor 32768
  "Port above which an unrostered, machine-held listener is display noise.
`bebop-backline-fleet' hides those rows and reports the count.
`bebop-backline-services' still returns them: the fleet data is
complete, only the table is choosy."
  :type 'integer
  :group 'bebop)

(defun bebop-backline--fleet-worth-showing-p (row)
  "Return non-nil if ROW earns a line in `bebop-backline-fleet'."
  (or (plist-get row :rostered)
      (not (equal (plist-get row :holder) bebop-backline-machine))
      (< (or (plist-get row :port) 0) bebop-backline-fleet-ephemeral-floor)))

(defun bebop-backline-fleet ()
  "Show this machine's service fleet and its venue work shells.
Every row is derived at read time — roster union lsof for the fleet,
tmux for the shells — so the buffer is a snapshot, not a registry."
  (interactive)
  (let ((buf (get-buffer-create "*bebop-backline*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Service fleet — %s\n\n" (bebop--host-name)))
        (insert (format "%-24s %-7s %-4s %-7s %s\n"
                        "SERVICE" "PORT" "GRP" "STATE" "HOLDER"))
        (let* ((all (bebop-backline-services))
               (rows (seq-filter #'bebop-backline--fleet-worth-showing-p all))
               (hidden (- (length all) (length rows))))
          (if (null rows)
              (insert "  (nothing listening, and no roster)\n")
            (dolist (r rows)
              (insert (format "%-24s %-7s %-4s %-7s %s\n"
                              (plist-get r :name)
                              (if (plist-get r :port)
                                  (format ":%d" (plist-get r :port))
                                "—")
                              (or (plist-get r :group) "—")
                              (cond ((plist-get r :up) "up")
                                    ((plist-get r :broken) "broken")
                                    (t "down"))
                              (or (plist-get r :holder) "—")))))
          (when (> hidden 0)
            (insert (format "\n  + %d machine-held listener%s above :%d\n"
                            hidden (if (= hidden 1) "" "s")
                            bebop-backline-fleet-ephemeral-floor))))
        (insert "\nVenue work shells\n\n")
        (let ((shells (bebop-backline-status)))
          (if (null shells)
              (insert "  (none)\n")
            (dolist (st shells)
              (insert (format "%-24s %-7s %s\n"
                              (plist-get st :slug)
                              (if (plist-get st :busy) "busy" "idle")
                              (let ((ports (plist-get st :ports)))
                                (if ports
                                    (mapconcat (lambda (p) (format ":%d" p))
                                               ports " ")
                                  (or (plist-get st :command) "—"))))))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun bebop-backline-takeover (port)
  "Free PORT after confirming with the user who currently holds it.
If a backline owns it, interrupt that backline's process. If an external
process owns it, offer to send it SIGTERM. Returns non-nil if PORT was
freed (or was already free)."
  (interactive "nTake over port: ")
  (let ((owner (bebop-backline-port-owner port)))
    (cond
     ((null owner)
      (message "Port %d is free" port)
      t)
     ((plist-get owner :slug)
      (let ((slug (plist-get owner :slug)))
        (when (y-or-n-p (format "Port %d held by backline %s (%s) — interrupt it? "
                                port slug
                                (or (bebop--backline-current-command slug) "?")))
          (bebop-backline-interrupt slug)
          t)))
     (t
      (let ((pid (plist-get owner :pid)))
        (when (y-or-n-p (format "Port %d held by external process %d — kill it? "
                                port pid))
          (signal-process pid 'TERM)
          t))))))

(defcustom bebop-backline-restore-on-release t
  "Whether releasing a service asks the machine to restore it.
When non-nil, `bebop-backline-release' runs the roster's :machine-cmd
after the session's process is gone, fire-and-forget. Set to nil on a
machine where a released service should simply stay down."
  :type 'boolean
  :group 'bebop)

(defun bebop-backline--venue-in-play ()
  "Return the venue path of the session in play, or nil.
The session at point wins over the active one — checking a service out
from a dashboard row should mean that row — and a buffer already
sitting inside a venue speaks for itself."
  (let* ((name (or (get-text-property (point) 'bebop-session-name)
                   (get-text-property (point) 'bebop-on-deck-name)
                   (bound-and-true-p bebop--active-session)))
         (venue (and name (plist-get (bebop--session-artifacts name) :venue))))
    (or venue
        (when-let ((slug (bebop--backline-venue-in-path
                          (expand-file-name default-directory))))
          (expand-file-name slug (expand-file-name bebop-venues-dir))))))

(defun bebop-backline--venue-repo (venue)
  "Return the repo VENUE is a worktree of, by the REPO--BRANCH convention."
  (let* ((slug (bebop--backline-slug-for-dir venue))
         (prefix (bebop--on-deck-prefix slug)))
    (if (string-empty-p prefix) slug prefix)))

(defun bebop-backline--read-service (prompt)
  "Read a fleet service name, completing over the rows worth offering."
  (let ((names (mapcar (lambda (r) (plist-get r :name))
                       (seq-filter #'bebop-backline--fleet-worth-showing-p
                                   (bebop-backline-services)))))
    (unless names
      (user-error "No services on the fleet: nothing listening, and no roster"))
    (completing-read prompt names nil t)))

(defun bebop-backline--await-free (port &optional tries)
  "Wait up to TRIES quarter-seconds for PORT to stop listening.
Return non-nil if it did."
  (let ((n (or tries 20)))
    (catch 'free
      (while (> n 0)
        (unless (bebop-backline-port-owner port) (throw 'free t))
        (setq n (1- n))
        (sleep-for 0.25))
      nil)))

(defun bebop-backline--boot (row)
  "Free ROW's port and return the holder that lost it, or nil.
A backline-held process is interrupted inside its own window: that
venue stops serving this service and keeps everything else, which is
the whole difference between checkout and `bebop-backline-kill'."
  (let ((pid (plist-get row :pid))
        (port (plist-get row :port))
        (holder (plist-get row :holder)))
    (when pid
      (if-let ((slug (bebop--backline-owning-slug pid)))
          (bebop--tmux "send-keys" "-t" (bebop--backline-window-id slug) "C-c")
        (signal-process pid 'TERM))
      (unless (bebop-backline--await-free port)
        (message "Backline: %s still holds :%d after the boot" holder port))
      holder)))

(defun bebop-backline-checkout (service &optional venue)
  "Take restart authority over SERVICE for the session in play.
VENUE defaults to the session at point, the active session, or the
venue this buffer sits in.

Boots whoever held SERVICE — naming them — then, if the roster knows a
:start-cmd and VENUE is a worktree of SERVICE's own repo, starts it in
VENUE's backline window. Otherwise the port is left free and the
command you need is reported: a venue cannot serve a repo it isn't."
  (interactive (list (bebop-backline--read-service "Check out service: ")))
  (let* ((venue (or venue (bebop-backline--venue-in-play)
                    (user-error "No session in play — check out from a session's row or venue")))
         (slug (bebop--backline-slug-for-dir venue))
         (row (or (bebop-backline--service-row service)
                  (user-error "No such service: %s" service)))
         (port (plist-get row :port))
         (start (plist-get row :start-cmd))
         (mine (equal (plist-get row :holder) slug))
         (booted (unless mine (bebop-backline--boot row))))
    (cond
     ((and mine (plist-get row :up))
      (message "Backline: %s already held by %s" service slug))
     ((and start (equal service (bebop-backline--venue-repo venue)))
      (bebop-backline-run venue start)
      (message "Backline: %s checked out by %s%s — running %s"
               service slug
               (if booted (format " (booted %s)" booted) "")
               start))
     (t
      (message "Backline: %s checked out by %s%s — %s"
               service slug
               (if booted (format " (booted %s)" booted) "")
               (cond
                ((null start) ":start-cmd unknown; start it yourself")
                (t (format "%s is not a %s venue; run %S where it belongs"
                           slug service start))))))
    (list :service service :holder slug :booted booted :port port)))

(defun bebop-backline--restore (row holder)
  "Ask the machine to restore ROW's service after HOLDER let it go."
  (let ((service (plist-get row :name))
        (cmd (plist-get row :machine-cmd)))
    (if (and cmd bebop-backline-restore-on-release)
        (progn
          (start-process-shell-command "bebop-backline-restore" nil cmd)
          (message "Backline: %s released by %s — machine restoring (%s)"
                   service holder cmd))
      (message "Backline: %s released by %s%s" service holder
               (if cmd " (restore-on-release off)" "")))))

(defun bebop-backline-release (service &optional holder)
  "Hand SERVICE back to the machine.
HOLDER is the venue slug releasing it, defaulting to the session in
play. Kills the process if HOLDER still holds SERVICE, then, when
`bebop-backline-restore-on-release' and the roster knows a
:machine-cmd, asks the machine to restore its own service —
fire-and-forget, because a restore takes as long as it takes.

A service some other venue holds is left alone and reported: releasing
is giving a service back, and you cannot give back what you never had."
  (interactive (list (bebop-backline--read-service "Release service: ")))
  (let* ((holder (or holder
                     (when-let ((v (bebop-backline--venue-in-play)))
                       (bebop--backline-slug-for-dir v))
                     (user-error "No session in play — release needs a holder")))
         (row (or (bebop-backline--service-row service)
                  (user-error "No such service: %s" service)))
         (current (plist-get row :holder)))
    (cond
     ((equal current holder)
      (bebop-backline--boot row)
      (bebop-backline--restore row holder))
     ((null current)
      (bebop-backline--restore row holder))
     (t
      (message "Backline: %s is held by %s, not %s — checkout takes it"
               service current holder)))))

(defun bebop--backline-dir-candidates ()
  "Return alist of (LABEL . DIR) candidates for backline selection:
venues of registered sessions first, then all venue worktrees on disk."
  (let (cands)
    (dolist (entry bebop--sessions)
      (when-let ((venue (plist-get (cdr entry) :venue)))
        (let ((label (bebop--backline-slug-for-dir venue)))
          (unless (assoc label cands)
            (push (cons label (expand-file-name venue)) cands)))))
    (let ((venues-dir (expand-file-name bebop-venues-dir)))
      (when (file-directory-p venues-dir)
        (dolist (f (directory-files venues-dir nil nil t))
          (when (and (not (string-prefix-p "." f))
                     (file-directory-p (expand-file-name f venues-dir))
                     (not (assoc f cands)))
            (push (cons f (expand-file-name f venues-dir)) cands)))))
    (nreverse cands)))

(defun bebop-backline (dir)
  "Open (or jump to) the backline work shell for DIR.
Interactively, picks from session venues and venue worktrees on disk.
Ensures the window exists, then selects it in tmux so any attached
client shows it."
  (interactive
   (let* ((cands (bebop--backline-dir-candidates))
          (choice (completing-read "Backline for venue: "
                                   (mapcar #'car cands) nil nil)))
     (list (or (cdr (assoc choice cands))
               (read-directory-name "Directory: " nil nil t choice)))))
  (let ((slug (bebop-backline-ensure dir)))
    (bebop--tmux "select-window" "-t" (bebop--backline-window-id slug))
    (message "Backline %s%s" slug
             (let ((ports (bebop--backline-ports slug)))
               (if ports
                   (format " — holding %s"
                           (mapconcat (lambda (p) (format ":%d" p)) ports " "))
                 "")))))

(provide 'bebop-backline)

;;; bebop-backline.el ends here
