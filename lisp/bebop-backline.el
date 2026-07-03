;;; bebop-backline.el --- Venue-owned work shells for long-running processes -*- lexical-binding: t; -*-

(require 'bebop-core)
(require 'bebop-dashboard)
(require 'bebop-session)

(defcustom bebop-backline-prefix "backline--"
  "tmux window-name prefix identifying backline (venue work shell) windows.
Must match the prefix skipped by `bebop--discover-existing-sessions'."
  :type 'string
  :group 'bebop)

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

(defun bebop--process-ancestors (pid)
  "Return PID's ancestor PIDs (nearest first), bounded against cycles."
  (let (result (current pid) (guard 0))
    (while (and current (> current 1) (< guard 64))
      (setq guard (1+ guard))
      (let ((ppid (with-temp-buffer
                    (when (eq 0 (call-process "ps" nil '(t nil) nil
                                              "-o" "ppid=" "-p"
                                              (number-to-string current)))
                      (let ((s (string-trim (buffer-string))))
                        (when (string-match-p "^[0-9]+$" s)
                          (string-to-number s)))))))
        (setq current (and ppid (> ppid 1) ppid))
        (when current (push current result))))
    (nreverse result)))

(defun bebop--backline-owning-slug (pid)
  "Return the backline slug whose pane shell is PID or an ancestor of PID."
  (let ((lineage (cons pid (bebop--process-ancestors pid))))
    (seq-find (lambda (slug)
                (let ((pane-pid (bebop--backline-pane-pid slug)))
                  (and pane-pid (memq pane-pid lineage))))
              (bebop--backline-slugs))))

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
          ports)
      (dolist (pair (bebop--listening-pairs))
        (let* ((pid (car pair))
               (lineage (or (gethash pid lineages)
                            (puthash pid
                                     (cons pid (bebop--process-ancestors pid))
                                     lineages))))
          (when (memq pane-pid lineage)
            (push (cdr pair) ports))))
      (nreverse ports))))

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
Keys: :slug :dir :busy :command :ports :last-command :started-at."
  (if slug
      (let ((meta (cdr (assoc slug bebop--backlines))))
        (list :slug slug
              :dir (plist-get meta :dir)
              :busy (and (bebop--backline-busy-p slug) t)
              :command (bebop--backline-current-command slug)
              :ports (bebop--backline-ports slug)
              :last-command (plist-get meta :last-command)
              :started-at (plist-get meta :started-at)))
    (mapcar #'bebop-backline-status (bebop--backline-slugs))))

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
