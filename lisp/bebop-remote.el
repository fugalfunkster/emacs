;;; bebop-remote.el --- Live Remote: drive a resident host's bebop over ssh -*- lexical-binding: t; -*-

(require 'seq)
(require 'url-parse)
(require 'bebop-core)
(require 'bebop-dashboard) ; the dot faces — same dots, same colors

(defcustom bebop-remote-host "matthews-mac-pro"
  "The resident host, as ssh reaches it — a tailnet name or ssh alias."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-user nil
  "User on the resident host, or nil for the local username."
  :type '(choice (const nil) string)
  :group 'bebop)

(defcustom bebop-remote-presence-host "Matthews-Pro"
  "The host's name in the mailbox protocol — its `bebop-call-host'.
Names the presence file; distinct from `bebop-remote-host' because
ssh reaches the machine by tailnet name while the mailbox knows it
by short hostname."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-emacsclient "/usr/local/bin/emacsclient"
  "Absolute path to emacsclient on the host."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-tmux "/usr/local/bin/tmux"
  "Absolute path to tmux on the host."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-tmux-session "claude"
  "The host's agent tmux session — its `bebop-tmux-session'."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-poll-interval 10
  "Seconds between remote dashboard refreshes while it is visible.
Coarser than the local 4s poll: each refresh is a network round trip."
  :type 'integer
  :group 'bebop)

(defcustom bebop-remote-ssh-options
  '("-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
    "-o" "ServerAliveInterval=5" "-o" "ServerAliveCountMax=2"
    "-o" "ControlMaster=auto" "-o" "ControlPath=~/.ssh/cm-%C"
    "-o" "ControlPersist=10m")
  "Options for every ssh invocation the thin client makes."
  :type '(repeat string)
  :group 'bebop)

(defun bebop-remote--target ()
  "The ssh destination, with the user prefix when one is configured."
  (if bebop-remote-user
      (format "%s@%s" bebop-remote-user bebop-remote-host)
    bebop-remote-host))

(defun bebop-remote--run (command)
  "Run COMMAND on the host over ssh. Return (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((code (apply #'call-process "ssh" nil t nil
                       (append bebop-remote-ssh-options
                               (list (bebop-remote--target) command)))))
      (cons code (string-trim (buffer-string))))))

(defun bebop-remote--eval (expr)
  "Evaluate EXPR in the host's Emacs; return the printed result.
Signals an error carrying ssh's own output on any failure."
  (let ((res (bebop-remote--run
              (format "%s --eval %s" bebop-remote-emacsclient
                      (shell-quote-argument expr)))))
    (unless (eq (car res) 0)
      (error "bebop-remote: %s"
             (if (string-empty-p (cdr res))
                 (format "ssh exit %s" (car res))
               (cdr res))))
    (cdr res)))

(defun bebop-remote--eval-json (expr)
  "Evaluate EXPR on the host, expecting a citizen-verb JSON string.
Returns parsed data: plists, vectors, strings; JSON null and false
both arrive as nil."
  (let ((printed (bebop-remote--eval expr)))
    (json-parse-string (car (read-from-string printed))
                       :object-type 'plist
                       :null-object nil
                       :false-object nil)))

(defun bebop-remote--presence-file ()
  "Path to the host's presence heartbeat in the local Dropbox, or nil."
  (when (fboundp 'bebop-call--default-dir)
    (when-let ((dir (bebop-call--default-dir)))
      (expand-file-name (format "%s.presence.json" bebop-remote-presence-host)
                        dir))))

(defun bebop-remote--presence ()
  "Return (PLIST . AGE-SECONDS) from the host's heartbeat, or nil."
  (when-let* ((file (bebop-remote--presence-file))
              (attrs (file-attributes file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (cons (json-parse-buffer :object-type 'plist
                                   :null-object nil :false-object nil)
                (float-time
                 (time-subtract (current-time)
                                (file-attribute-modification-time attrs)))))
      (error nil))))

(defvar bebop-remote-buffer-name "*bebop-remote*"
  "Buffer name for the remote dashboard.")

(defvar bebop-remote--active nil
  "The far session you selected with RET, or nil.
The remote counterpart of `bebop--active-session'.")

(defvar bebop-remote--sessions nil
  "Rows from the last snapshot (a list of plists), or nil.")

(defvar bebop-remote--body nil
  "The host's last rendered tree, a propertized string, or nil.")

(defvar bebop-remote--folds nil
  "Alist of section value → `hide' or `show', for sections you toggled.")

(defvar bebop-remote--error nil
  "Error string from the last failed refresh, or nil when reachable.")

(defvar bebop-remote--timer nil)

(defvar bebop-remote--fetch nil "The snapshot fetch in flight, or nil.")

(defvar bebop-remote--refetch nil
  "Non-nil when a refresh was asked for while a fetch was in flight.")

(defun bebop-remote--snapshot-expr ()
  "The host expression that renders the dashboard for this client."
  (format "(bebop-dashboard-snapshot %S '%S)"
          bebop-remote--active bebop-remote--folds))

(defvar bebop-remote--pulse nil
  "The host's pulse health from the last snapshot (`bebop--pulse-health').")

(defun bebop-remote--take-snapshot (printed)
  "Install the host's reply, PRINTED as emacsclient returned it."
  (let ((p (car (read-from-string (car (read-from-string printed))))))
    (setq bebop-remote--body (plist-get p :body)
          bebop-remote--sessions (plist-get p :rows)
          bebop-remote--pulse (plist-get p :pulse)
          bebop-remote--error nil)))

(defun bebop-remote-pulse-run ()
  "Start a pulse run on the host, then refresh to show it going."
  (interactive)
  (bebop-remote--eval "(progn (bebop-pulse-run) t)")
  (bebop-remote-refresh))

(defun bebop-remote--fetch-failed (msg)
  (setq bebop-remote--error msg)
  (bebop-remote--stop-timer))

(defun bebop-remote--fetch-sentinel (proc _event)
  "Install a finished fetch, redraw, and run a refresh that waited."
  (unless (process-live-p proc)
    (let* ((buf (process-buffer proc))
           (out (with-current-buffer buf (string-trim (buffer-string)))))
      (kill-buffer buf)
      (when (eq proc bebop-remote--fetch)
        (setq bebop-remote--fetch nil))
      (if (eq (process-exit-status proc) 0)
          (condition-case err
              (progn (bebop-remote--take-snapshot out)
                     (bebop-remote--start-timer))
            (error (bebop-remote--fetch-failed (error-message-string err))))
        (bebop-remote--fetch-failed
         (if (string-empty-p out)
             (format "ssh exit %s" (process-exit-status proc))
           out)))
      (bebop-remote--render)
      (when bebop-remote--refetch
        (bebop-remote-refresh)))))

(defun bebop-remote--mark-names (beg end)
  "Give each session line between BEG and END a `bebop-remote-name'."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when-let ((name (or (get-text-property (point) 'bebop-session-name)
                           (get-text-property (point) 'bebop-on-deck-name))))
        (put-text-property (point) (line-beginning-position 2)
                           'bebop-remote-name name))
      (forward-line 1))))

(defun bebop-remote--render ()
  "Redraw the remote dashboard from the last snapshot."
  (let ((buf (get-buffer-create bebop-remote-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'bebop-remote-mode)
        (bebop-remote-mode))
      (let ((inhibit-read-only t)
            (saved-name (get-text-property (point) 'bebop-remote-name))
            (saved-line (line-number-at-pos)))
        (erase-buffer)
        (cond
         (bebop-remote--error
          (insert (propertize
                   (format "  %s unreachable — g to retry\n  %s\n"
                           bebop-remote-host bebop-remote--error)
                   'face 'shadow)))
         ((null bebop-remote--body)
          (insert (propertize "  Fetching…\n" 'face 'shadow)))
         (t
          (bebop-remote--notice-edges)
          (insert bebop-remote--body)
          (bebop-remote--mark-names (point-min) (point))))
        (insert "\n")
        (dolist (line '("RET: select  TAB: fold  h: host  g: refresh  .: mark seen"
                        "n: new  k: kill  r: resume  e: exile"
                        "C/D: chart  V/W: venue  N: new set  m: move to set"
                        "w: pin W  L: workbench layout  a: attach (mosh)  P: forward port"
                        "f: followed up (ack ↪ handoffs)  o: open MR in browser (derived sets)"))
          (insert (propertize (concat line "\n")
                              'face '(:inherit shadow :height 0.75))))
        (goto-char (point-min))
        (let ((found nil))
          (when saved-name
            (while (and (not found) (< (point) (point-max)))
              (if (equal (get-text-property (point) 'bebop-remote-name)
                         saved-name)
                  (setq found t)
                (forward-line 1))))
          (unless found
            (goto-char (point-min))
            (forward-line (1- saved-line))))))))

(defun bebop-remote-refresh (&optional sync)
  "Fetch the host's dashboard and redraw; restart polling on success.
SYNC waits for the reply; otherwise the fetch runs in the background."
  (interactive)
  (cond
   (sync
    (condition-case err
        (progn
          (bebop-remote--take-snapshot
           (bebop-remote--eval (bebop-remote--snapshot-expr)))
          (bebop-remote--start-timer))
      (error (bebop-remote--fetch-failed (error-message-string err))))
    (bebop-remote--render))
   ((process-live-p bebop-remote--fetch)
    (setq bebop-remote--refetch t))
   (t
    (setq bebop-remote--refetch nil
          bebop-remote--fetch
          (make-process
           :name "bebop-remote-fetch"
           :buffer (generate-new-buffer " *bebop-remote-fetch*")
           :command (append (list "ssh") bebop-remote-ssh-options
                            (list (bebop-remote--target)
                                  (format "%s --eval %s" bebop-remote-emacsclient
                                          (shell-quote-argument
                                           (bebop-remote--snapshot-expr)))))
           :connection-type 'pipe
           :noquery t
           :sentinel #'bebop-remote--fetch-sentinel)))))

(defun bebop-remote-toggle ()
  "Fold or unfold the set heading at point; the host re-renders."
  (interactive)
  (let ((fold (get-text-property (point) 'bebop-fold)))
    (unless fold
      (user-error "No set heading at point"))
    (setf (alist-get (car fold) bebop-remote--folds nil nil #'equal)
          (if (cdr fold) 'show 'hide))
    (bebop-remote-refresh)))

(defun bebop-remote--poll ()
  (let ((buf (get-buffer bebop-remote-buffer-name)))
    (if (buffer-live-p buf)
        (when (get-buffer-window buf t)
          (bebop-remote-refresh))
      (bebop-remote--stop-timer))))

(defun bebop-remote--start-timer ()
  "Start the remote refresh: a loop that keeps running."
  (unless (bebop--loop-running-p 'remote)
    (bebop--loop-start 'remote bebop-remote-poll-interval #'bebop-remote--poll)))

(defun bebop-remote--stop-timer ()
  (bebop--loop-stop 'remote))

;; Until 2026-11-06 a plain repeating timer; a reload moves it onto the loop.
(when (timerp bebop-remote--timer)
  (cancel-timer bebop-remote--timer)
  (setq bebop-remote--timer nil)
  (bebop-remote--start-timer))


(defun bebop-remote--nav-stop-p ()
  "Non-nil on a line the arrow keys stop at: sessions, derived-set
headings and their MR rows (see `bebop--nav-stop-p')."
  (or (get-text-property (point) 'bebop-remote-name)
      (get-text-property (point) 'bebop-mr-item)
      (get-text-property (point) 'bebop-mr-heading)))

(defun bebop-remote--next ()
  "Move point to the next session line."
  (interactive)
  (let ((start (point)))
    (when (bebop-remote--nav-stop-p)
      (forward-line 1))
    (while (and (< (point) (point-max))
                (null (bebop-remote--nav-stop-p)))
      (forward-line 1))
    (unless (bebop-remote--nav-stop-p)
      (goto-char start))))

(defun bebop-remote--prev ()
  "Move point to the previous session line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (> (point) (point-min))
                (null (bebop-remote--nav-stop-p)))
      (forward-line -1))
    (unless (bebop-remote--nav-stop-p)
      (goto-char start))))

(defvar bebop-remote-mode-map (make-sparse-keymap)
  "Keymap for `bebop-remote-mode'.")

;; Bound at top level, not inside the defvar: a reload never re-runs a
;; defvar, so keys added there never reached a running client. The
;; iMac's map stayed the first version's until 2026-11-05 — TAB, o, k,
;; e and the rest fell through to self-insert ("Buffer is read-only").
(dolist (b '(
           ("g" . bebop-remote-refresh)
           ("RET" . bebop-remote-select)
           ("o" . bebop-remote-browse)
           ("h" . bebop-dashboard-next-tab)
           ("TAB" . bebop-remote-toggle)
           ("<tab>" . bebop-remote-toggle)
           ("a" . bebop-remote-attach)
           ("P" . bebop-remote-forward-port)
           ("G" . bebop-remote-pulse-run)
           ("." . bebop-remote-mark-all-seen)
           ("f" . bebop-remote-ack-handoff)
           ("w" . bebop-remote-workbench)
           ("L" . bebop-remote-workbench-layout)
           ("e" . bebop-remote-exile)
           ("C" . bebop-remote-associate-chart)
           ("D" . bebop-remote-dissociate-chart)
           ("V" . bebop-remote-associate-venue)
           ("W" . bebop-remote-dissociate-venue)
           ("N" . bebop-remote-new-set)
           ("m" . bebop-remote-assign-set)
           ("r" . bebop-remote-resume)
           ("k" . bebop-remote-kill)
           ("n" . bebop-remote-new-session)
           ("j" . bebop-remote--next)
           ("p" . bebop-remote--prev)
           ("<down>" . bebop-remote--next)
           ("<up>" . bebop-remote--prev)))
  (define-key bebop-remote-mode-map (kbd (car b)) (cdr b)))

(define-derived-mode bebop-remote-mode special-mode "BebopRemote"
  "Dashboard for a resident host's session pool, rendered over ssh.
\\{bebop-remote-mode-map}"
  (bebop--dashboard-look)
  (setq header-line-format
        '((:eval
           (concat
            ;; The same header as *bebop*, tabs inline: the tab names
            ;; the machine, and switching tabs must not shift the tree.
            (bebop--dashboard-header)
            (unless (process-live-p bebop-remote--stream)
              (propertize "  stream down" 'face 'shadow))
            (when bebop-remote--forwards
              (propertize
               (concat "  ⇄ "
                       (mapconcat (lambda (f)
                                    (format "%s%d" (if (equal (car f) "-R") "↑" "")
                                            (cdr f)))
                                  (reverse bebop-remote--forwards) " "))
               'face 'shadow))
            (bebop--pulse-segment bebop-remote--pulse #'bebop-remote-pulse-run)))))
  (add-hook 'kill-buffer-hook #'bebop-remote--stop-timer nil t)
  (add-hook 'kill-buffer-hook #'bebop-remote--stop-stream nil t)
  ;; A host tab coming up (or any other display) shows fresh state.
  (add-hook 'window-buffer-change-functions
            (lambda (_win) (bebop-remote-refresh)) nil t)
  )

;;;###autoload
(defun bebop-remote ()
  "Open the Live Remote dashboard for `bebop-remote-host'.
Reads the host's presence heartbeat first — awake-or-not is answered
from the local Dropbox before any network call."
  (interactive)
  (when-let ((p (bebop-remote--presence)))
    (let ((data (car p))
          (age (cdr p))
          (interval (if (boundp 'bebop-presence-interval)
                        bebop-presence-interval
                      300)))
      (if (> age (* 3 interval))
          (message "bebop-remote: presence is %dm stale — far Emacs may be down; trying anyway"
                   (/ (round age) 60))
        (message "bebop-remote: %s awake, %s live session(s)"
                 (or (plist-get data :host) bebop-remote-presence-host)
                 (or (plist-get data :live) "?")))))
  (pop-to-buffer (get-buffer-create bebop-remote-buffer-name))
  (unless (derived-mode-p 'bebop-remote-mode)
    (bebop-remote-mode))
  (bebop-remote-refresh)
  (bebop-remote--start-stream))

(defvar-local bebop-remote--compose-session nil
  "The far session this composition buffer sends to, or nil.")

(defun bebop-remote--window-name (name)
  "The host tmux window for session NAME (strip a leading /)."
  (if (string-prefix-p "/" name)
      (substring name 1)
    name))

(defun bebop-remote-send-buffer ()
  "Send the buffer to the far session's pane and submit, then clear."
  (interactive)
  (unless bebop-remote--compose-session
    (user-error "Not a bebop-remote composition buffer"))
  (let* ((target (format "%s:%s" bebop-remote-tmux-session
                         (bebop-remote--window-name
                          bebop-remote--compose-session)))
         (qt (shell-quote-argument target))
         (enter (format "%s send-keys -t %s C-m" bebop-remote-tmux qt))
         ;; As `bebop--tmux-paste': a named buffer, bracketed, and a
         ;; blank buffer sends only the Enter. The pause matters too: on
         ;; the host the tmux calls land in microseconds, and an Enter
         ;; inside the paste burst can read as a newline, not a submit.
         (cmd (if (string-blank-p (buffer-string))
                  enter
                (format "%s load-buffer -b bebop-send - && %s paste-buffer -b bebop-send -d -p -t %s && sleep 0.3 && %s"
                        bebop-remote-tmux bebop-remote-tmux qt enter)))
         (code (apply #'call-process-region (point-min) (point-max)
                      "ssh" nil nil nil
                      (append bebop-remote-ssh-options
                              (list (bebop-remote--target) cmd)))))
    (if (eq code 0)
        (progn
          (erase-buffer)
          (message "Sent to %s on %s"
                   bebop-remote--compose-session bebop-remote-host))
      (user-error "bebop-remote: send failed (ssh exit %s)" code))))

(defvar bebop-remote-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'bebop-remote-send-buffer)
    map)
  "Keymap for `bebop-remote-compose-mode'.")

(define-minor-mode bebop-remote-compose-mode
  "Composition buffer for a far session; \\[bebop-remote-send-buffer] sends over ssh."
  :lighter " Remote"
  :keymap bebop-remote-compose-mode-map)

(defun bebop-remote-kill (&optional name)
  "Kill far session NAME (or the one at point) on the host.
Confirms as `k' does locally, by typing the name or \"see ya\"; the
host then pauses it without prompts (`bebop-stop-session')."
  (interactive)
  (let* ((name (or name
                   (get-text-property (point) 'bebop-remote-name)
                   (user-error "No session at point")))
         (s (seq-find (lambda (s) (equal (plist-get s :name) name))
                      bebop-remote--sessions)))
    (unless (plist-get s :live)
      (user-error "%s is not running on %s" name bebop-remote-host))
    (let ((typed (read-string
                  (format "Type \"%s\" (or \"see ya\") to kill it on %s: "
                          name bebop-remote-host))))
      (unless (member typed (list name "see ya"))
        (user-error "Cancelled (name mismatch)")))
    (bebop-remote--eval-json (format "(bebop-stop-session %S)" name))
    (when (equal name bebop-remote--active)
      (setq bebop-remote--active nil))
    (message "Bebop: killed %s on %s" name bebop-remote-host)
    (bebop-remote-refresh)))

(defun bebop-remote-resume (&optional name)
  "Resume the far on-deck session NAME (or the one at point) on the host."
  (interactive)
  (let* ((name (or name
                   (get-text-property (point) 'bebop-remote-name)
                   (user-error "No session at point")))
         (s (seq-find (lambda (s) (equal (plist-get s :name) name))
                      bebop-remote--sessions)))
    (when (plist-get s :live)
      (user-error "%s is already running on %s" name bebop-remote-host))
    (bebop-remote--eval-json (format "(bebop-resume-session %S)" name))
    (message "Bebop: resumed %s on %s" name bebop-remote-host)
    (bebop-remote-refresh)))

(defun bebop-remote--compose-buffer (name)
  "Return far session NAME's composition buffer, set up."
  (let ((buf (get-buffer-create (format "*bebop-remote: %s*" name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (when (fboundp 'bebop-composition-mode)
        (bebop-composition-mode 1))
      (bebop--apply-buffer-settings)
      (setq bebop-remote--compose-session name)
      (bebop-remote-compose-mode 1)
      ;; The header line names the session; a buffer made before it did
      ;; still carries the old second header, "Remote → NAME".
      (when (overlayp bebop--header-overlay)
        (delete-overlay bebop--header-overlay)
        (setq bebop--header-overlay nil)))
    buf))

(defun bebop-remote--act-item (item)
  "Act on derived-set ITEM on the host, as RET does locally.
Reviewing jams the host's review session and selects it here.
Authored selects the MR's session, or asks here and starts one there."
  (pcase (plist-get item :set)
    ("Reviewing"
     (let ((session (car (read-from-string
                          (bebop-remote--eval
                           (format "(bebop-derived-review '%S)" item))))))
       (bebop-remote-select session)
       (message "Bebop: %s on %s asked to review %s"
                session bebop-remote-host (plist-get item :ref))))
    ("Authored"
     (if-let ((session (plist-get item :session)))
         (bebop-remote-select session)
     (let ((name (format "%s--%s"
                         (car (last (split-string (plist-get item :project) "/")))
                         (bebop--sanitize-name (plist-get item :branch)))))
       (when (y-or-n-p (format "Start %s on %s? " name bebop-remote-host))
         (bebop-remote--eval (format "(bebop-derived-start '%S)" item))
         (bebop-remote-refresh)
         (message "Bebop: started %s on %s" name bebop-remote-host)))))))

(defun bebop-remote-browse ()
  "Open the MR at point in this machine's browser."
  (interactive)
  (if-let ((item (get-text-property (point) 'bebop-mr-item)))
      (browse-url (plist-get item :url))
    (user-error "No merge request at point")))

(defun bebop-remote-select (&optional name)
  "Select far session NAME (or the one at point), as RET does locally.
Its composition buffer takes the Conductor's composition window (or
another window when there is no Conductor), the D terminal switches to
it, and it counts as seen. Moving point alone changes nothing. On a
derived-set row, RET acts on the MR instead (`bebop-remote--act-item')."
  (interactive)
  (if-let ((item (and (null name)
                      (null (get-text-property (point) 'bebop-remote-name))
                      (get-text-property (point) 'bebop-mr-item))))
      (bebop-remote--act-item item)
    (bebop-remote--select-session name)))

(defun bebop-remote--select-session (name)
  "Select far session NAME, or the one at point. See `bebop-remote-select'."
  (let* ((name (or name
                   (get-text-property (point) 'bebop-remote-name)
                   (user-error "No session at point")))
         (s (seq-find (lambda (s) (equal (plist-get s :name) name))
                      bebop-remote--sessions)))
    (unless (plist-get s :live)
      (user-error "%s is on deck — r resumes it on %s" name bebop-remote-host))
    (setq bebop-remote--active name)
    (let* ((buf (bebop-remote--compose-buffer name))
           (conductor (seq-find (lambda (f) (frame-parameter f 'bebop-conductor))
                                (frame-list)))
           (win (and conductor (frame-parameter conductor 'bebop-comp-window))))
      (if (window-live-p win)
          (progn (set-window-buffer win buf)
                 (with-selected-window win (text-scale-set 3)))
        (display-buffer buf '((display-buffer-use-some-window
                               display-buffer-pop-up-window)
                              (inhibit-same-window . t)))))
    (when (bebop--emacs-focused-p)
      (bebop-remote-steer "D" name))
    ;; The host acks the selected session as it renders.
    (bebop-remote-refresh)
    (message "Bebop: %s on %s — compose, then C-c C-c to send"
             name bebop-remote-host)))

(defun bebop-remote-compose (&optional name)
  "Open far session NAME's composition buffer in the selected window."
  (interactive)
  (let ((name (or name
                  (get-text-property (point) 'bebop-remote-name)
                  (user-error "No session at point"))))
    (pop-to-buffer (bebop-remote--compose-buffer name))
    (bebop-remote--eval (format "(bebop-set-ack-session %S)" name))
    (when (get-buffer bebop-remote-buffer-name)
      (bebop-remote-refresh))))

(defcustom bebop-remote-events-dir "~/.emacs.d-local/bebop-events"
  "The host's hook event directory — its `bebop-events-dir'."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-python "/usr/bin/python3"
  "Absolute path to python3 on the host."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-stream-retry 5
  "Seconds before a dropped event stream reconnects."
  :type 'integer
  :group 'bebop)

(defvar bebop-remote-stream-command nil
  "Command list that starts the event stream, or nil for ssh to the host.
The program arrives on its standard input. Tests set this to run the
stream locally.")

(defvar bebop-remote--hook-states (make-hash-table :test #'equal)
  "Event file stem on the host → folded hook state, as `bebop--hook-states'.")

(defvar bebop-remote--stream nil "The event stream process, or nil.")
(defvar bebop-remote--stream-render-timer nil)
(defvar bebop-remote--stream-retry-timer nil)

(defvar bebop-remote--forwards nil
  "Forwards to keep on the connection: a list of (FLAG . PORT).
FLAG is \"-L\" (a host port reachable here) or \"-R\" (a port here
reachable on the host). A dropped connection loses them, so the event
stream sets them up again each time it connects.")

(defvar bebop-remote--quiet-until 0
  "Edges before this time (a stream replay) do not alert. See Attention.")

(defun bebop-remote--stream-script ()
  "The stream program's text, from this machine's own tangled copy."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "bin/bebop-event-stream" user-emacs-directory))
    (buffer-string)))

(defun bebop-remote--stream-filter (proc output)
  "Fold each whole line of stream OUTPUT; refresh once the burst ends."
  (let ((buf (concat (or (process-get proc :partial) "") output))
        (start 0))
    (while (string-match "\n" buf start)
      (let* ((line (substring buf start (match-beginning 0)))
             (tab (string-search "\t" line)))
        (setq start (match-end 0))
        (when-let* ((tab tab)
                    (stem (substring line 0 tab))
                    (ev (ignore-errors
                          (json-parse-string (substring line (1+ tab))
                                             :object-type 'plist :array-type 'list
                                             :null-object nil :false-object nil))))
          (puthash stem
                   (if (equal (plist-get ev :e) "SessionStart")
                       (bebop--hook-fold nil ev)
                     (bebop--hook-fold (gethash stem bebop-remote--hook-states) ev))
                   bebop-remote--hook-states))))
    (process-put proc :partial (substring buf start))
    (when (timerp bebop-remote--stream-render-timer)
      (cancel-timer bebop-remote--stream-render-timer))
    (setq bebop-remote--stream-render-timer
          (run-at-time 0.3 nil #'bebop-remote-refresh))))

(defun bebop-remote--stream-sentinel (proc _event)
  "Reconnect a dropped stream while the remote dashboard lives."
  (unless (process-live-p proc)
    (when (eq proc bebop-remote--stream)
      (setq bebop-remote--stream nil)
      (when (get-buffer bebop-remote-buffer-name)
        (bebop-remote--render)
        (setq bebop-remote--stream-retry-timer
              (run-at-time bebop-remote-stream-retry nil
                           #'bebop-remote--start-stream))))))

(defun bebop-remote--start-stream ()
  "Open the event stream unless it is already open."
  (unless (process-live-p bebop-remote--stream)
    ;; A replay rebuilds every state, so start from nothing, and its
    ;; edges are old news (see Attention).
    (clrhash bebop-remote--hook-states)
    (setq bebop-remote--quiet-until (+ (float-time) 5))
    (let* ((args (list "-u" "-" bebop-remote-events-dir))
           (cmd (or bebop-remote-stream-command
                    (append (list "ssh") bebop-remote-ssh-options
                            (list (bebop-remote--target)
                                  (mapconcat #'shell-quote-argument
                                             (cons bebop-remote-python args)
                                             " ")))))
           (proc (make-process :name "bebop-remote-stream"
                               :command (if bebop-remote-stream-command
                                            (append cmd args)
                                          cmd)
                               :connection-type 'pipe
                               :noquery t
                               :filter #'bebop-remote--stream-filter
                               :sentinel #'bebop-remote--stream-sentinel)))
      (process-send-string proc (bebop-remote--stream-script))
      (process-send-eof proc)
      (setq bebop-remote--stream proc)
      ;; Forwards live on the connection; a new one needs them again.
      (when (and bebop-remote--forwards (not bebop-remote-stream-command))
        (run-at-time 2 nil #'bebop-remote--reforward)))))

(defun bebop-remote--stop-stream ()
  "Close the event stream and its timers."
  (dolist (tm (list bebop-remote--stream-retry-timer
                    bebop-remote--stream-render-timer))
    (when (timerp tm) (cancel-timer tm)))
  (let ((proc bebop-remote--stream))
    (setq bebop-remote--stream nil)
    (when (process-live-p proc)
      (delete-process proc))))

(defun bebop-remote--hook (name)
  "The far session NAME's effective hook state, or nil without events."
  (when-let ((st (gethash (replace-regexp-in-string
                           "/" "_" (string-remove-prefix "/" name))
                          bebop-remote--hook-states)))
    (when (plist-get st :state)
      (cons (bebop--hook-effective st) (plist-get st :at)))))

(defvar bebop-remote--seen-attention t
  "Names that held attention at the last snapshot; t before the first.")

(defvar bebop-remote--periphery nil
  "Cached mode-line segment for far attention, or nil when there is none.")

(defun bebop-remote-ack-handoff ()
  "Ack the far session's handoffs at point; offer a resend of dropped ones.
The question runs here and the work runs on the host, which keeps the
ledger (see Handoffs)."
  (interactive)
  (let* ((name (bebop-remote--name-at-point))
         (hs (append (bebop-remote--eval-json
                      (format "(bebop-handoff-list %S)" name))
                     nil))
         (dropped (seq-count (lambda (h) (equal (plist-get h :state) "dropped"))
                             hs)))
    (if (null hs)
        (message "Bebop: %s has no handoffs" name)
      (bebop-remote--eval
       (format "(bebop-handoff-settle %S %S)" name
               (and (> dropped 0)
                    (y-or-n-p (format "Send %d dropped task%s to %s again? "
                                      dropped (if (= dropped 1) "" "s") name))
                    t)))
      (bebop-remote-refresh)
      (message "Bebop: %s followed up" name))))

(defun bebop-remote-mark-all-seen ()
  "Mark every far session seen, on the host."
  (interactive)
  (bebop-remote--eval "(bebop-mark-all-seen)")
  (bebop-remote-refresh)
  (message "Bebop: all %s sessions marked seen" bebop-remote-host))

(defun bebop-remote--watching-p (name)
  "Non-nil if far session NAME is the one you selected (RET)."
  (equal name bebop-remote--active))

(defun bebop-remote--notice-edges ()
  "Alert on far sessions that gained attention; refresh the mode line.
Called before each render of a fresh snapshot."
  (let* ((quiet (< (float-time) bebop-remote--quiet-until))
         (now (seq-filter (lambda (s) (plist-get s :attention))
                          bebop-remote--sessions))
         (names (mapcar (lambda (s) (plist-get s :name)) now)))
    (unless (or quiet (eq bebop-remote--seen-attention t))
      (dolist (s now)
        (let ((name (plist-get s :name)))
          (unless (or (member name bebop-remote--seen-attention)
                      (bebop-remote--watching-p name))
            (let ((msg (plist-get (car (bebop-remote--hook name)) :msg))
                  (status (plist-get s :status)))
              (bebop-alert)
              (message "Bebop: %s on %s %s%s" name bebop-remote-host
                       (if (member status '("waiting" "blocked"))
                           (concat "is " status)
                         "needs a look")
                       (if msg (format " — %s" msg) "")))))))
    (setq bebop-remote--seen-attention names)
    (setq bebop-remote--periphery
          (when now
            (concat " " (propertize (format "%s ● %d"
                                            ;; "Matthews-Pro" → "Pro"
                                            (car (last (split-string
                                                        bebop-remote-presence-host
                                                        "-")))
                                            (length now))
                                    'face 'bebop-dot-waiting-face)
                    " "))))
  (force-mode-line-update t))

(defun bebop-remote--modeline ()
  "The far attention segment for `global-mode-string'."
  (or bebop-remote--periphery ""))

(add-to-list 'global-mode-string '(:eval (bebop-remote--modeline)) t)

(defcustom bebop-remote-client-name (car (split-string (system-name) "\\."))
  "This machine's name in the grouped session names: `hostname -s'.
bin/bebop-term names each window's grouped session CLIENT-ROLE."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-term-program "bebop-term"
  "The dotfiles program that opens a role's terminal window."
  :type 'string
  :group 'bebop)

(defvar bebop-remote-host-command nil
  "Command list that runs a shell command on the host, or nil for ssh.
Tests set this to (\"sh\" \"-c\") to steer the local tmux.")

(defvar bebop-remote--missing-roles nil
  "Roles whose terminal was found missing, so the notice shows once.")

(defun bebop-remote--host-argv (command)
  "The argv that runs shell COMMAND on the host."
  (if bebop-remote-host-command
      (append bebop-remote-host-command (list command))
    (append (list "ssh") bebop-remote-ssh-options
            (list (bebop-remote--target) command))))

(defun bebop-remote--term-tab (role host)
  "Bring ROLE's Ghostty tab for HOST to the front, without blocking.
HOST is \"local\" or an ssh destination, as `bebop-term' takes it."
  (when (executable-find bebop-remote-term-program)
    (call-process bebop-remote-term-program nil 0 nil "-s" role host)))

(defun bebop--emacs-focused-p ()
  "Non-nil when an Emacs frame has keyboard focus.
Terminals follow the selection only then. When you are typing in a
Ghostty window, a selection made elsewhere (an agent, a timer, the
other machine) must not switch or raise it under you (2026-11-06)."
  (seq-some (lambda (f) (eq t (frame-focus-state f))) (frame-list)))

(defun bebop-remote--steer-local ()
  "Steer this machine's own D tab to the active local session."
  (when (and bebop--active-session (bebop--emacs-focused-p))
    (call-process "tmux" nil 0 nil "select-window" "-t"
                  (format "%s-D:=%s" bebop-remote-client-name
                          (bebop--tmux-window-name bebop--active-session)))
    (bebop-remote--term-tab "D" "local")))

(advice-add 'bebop--apply-active-session :after #'bebop-remote--steer-local)

(defun bebop-remote-steer (role name &optional callback)
  "Show far session NAME in the ROLE terminal, without blocking.
CALLBACK, if given, gets t on success and nil when that terminal's
grouped session does not exist."
  (let* ((target (format "%s-%s:=%s" bebop-remote-client-name role
                         (bebop-remote--window-name name)))
         (cmd (format "%s select-window -t %s" bebop-remote-tmux
                      (shell-quote-argument target))))
    (make-process
     :name (format "bebop-remote-steer-%s" role)
     :command (bebop-remote--host-argv cmd)
     :connection-type 'pipe
     :noquery t
     :buffer nil
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (let ((ok (eq 0 (process-exit-status proc))))
                     (if ok
                         (progn
                           (setq bebop-remote--missing-roles
                                 (delete role bebop-remote--missing-roles))
                           (bebop-remote--term-tab role (bebop-remote--target)))
                       (unless (or callback
                                   (member role bebop-remote--missing-roles))
                         (push role bebop-remote--missing-roles)
                         (message "Bebop: no %s terminal for %s — run %s %s %s"
                                  role name bebop-remote-term-program role
                                  (bebop-remote--target))))
                     (when callback (funcall callback ok))))))))

(defun bebop-remote-open-terminal (role &rest ghostty-args)
  "Open the ROLE terminal window with `bebop-remote-term-program'.
GHOSTTY-ARGS go to Ghostty, for example its window position."
  (unless (executable-find bebop-remote-term-program)
    (user-error "Bebop: %s is not on PATH (dotfiles bin/)" bebop-remote-term-program))
  (apply #'call-process bebop-remote-term-program nil 0 nil role
         (bebop-remote--target) ghostty-args))

(defun bebop-remote-workbench (&optional name)
  "Pin the W terminal to far session NAME (or the one at point).
Opens the W window first when it does not exist yet."
  (interactive)
  (let ((name (or name
                  (get-text-property (point) 'bebop-remote-name)
                  (user-error "No session at point"))))
    (bebop-remote-steer
     "W" name
     (lambda (ok)
       (if ok
           (message "Bebop: workbench terminal → %s" name)
         (bebop-remote-open-terminal "W")
         ;; mosh needs a moment to attach before the session exists.
         (run-at-time 3 nil #'bebop-remote-steer "W" name
                      (lambda (ok)
                        (message (if ok "Bebop: workbench terminal → %s"
                                   "Bebop: W terminal did not come up for %s")
                                 name))))))))

(defcustom bebop-remote-workbench-split '(0.42 . 0.4)
  "Workbench proportions: (LEFT-WIDTH . CHART-HEIGHT), as fractions.
LEFT-WIDTH is the chart-and-terminal column's share of the screen;
CHART-HEIGHT is the chart's share of that column."
  :type '(cons number number)
  :group 'bebop)

(defcustom bebop-remote-term-cell '(9 . 19)
  "Estimated Ghostty cell size in pixels, (WIDTH . HEIGHT).
Ghostty takes its initial size in cells; this converts."
  :type '(cons integer integer)
  :group 'bebop)

(defcustom bebop-remote-dev-ports '(("web-app" . 3000) ("commander" . 3002))
  "Dev server port by repo, for sessions without a backline port.
The repo is the session name's part before \"--\"."
  :type '(alist :key-type string :value-type integer)
  :group 'bebop)

(defcustom bebop-remote-chrome-app "Google Chrome"
  "The Chrome application the workbench launches."
  :type 'string
  :group 'bebop)

(defcustom bebop-remote-chrome-profile "~/.emacs.d-local/bebop-chrome"
  "Profile directory of the workbench Chrome.
Its own profile, so logins persist and your everyday Chrome is
untouched. Chrome also refuses remote debugging on its default
profile."
  :type 'directory
  :group 'bebop)

(defcustom bebop-remote-chrome-debug-port 9333
  "The workbench Chrome's debug port, here and, reverse-forwarded, on the host."
  :type 'integer
  :group 'bebop)

(defun bebop-remote--workarea ()
  "The workbench screen's work area (X Y W H): the second screen if any.
Without a second screen, the main one. The height is the smallest that
any frame on that screen reports: the NS port gave a work area without
the Dock once (Chrome ran 91 px past the bottom on 2026-11-04), likely
read from a frame that hides the Dock, so one reading is not trusted."
  (let* ((mons (display-monitor-attributes-list))
         (mon (or (car (last (cdr mons))) (car mons)))
         (geometry (alist-get 'geometry mon))
         (area (alist-get 'workarea mon))
         (heights
          (delq nil
                (mapcar (lambda (f)
                          (let ((m (frame-monitor-attributes f)))
                            (when (equal (alist-get 'geometry m) geometry)
                              (nth 3 (alist-get 'workarea m)))))
                        (visible-frame-list)))))
    (list (nth 0 area) (nth 1 area) (nth 2 area)
          (apply #'min (nth 3 area) heights))))

(defun bebop-remote--workbench-boxes ()
  "Pixel boxes (X Y W H) for the chart, terminal and browser."
  (pcase-let* ((`(,x ,y ,w ,h) (bebop-remote--workarea))
               (lw (round (* w (car bebop-remote-workbench-split))))
               (ch (round (* h (cdr bebop-remote-workbench-split)))))
    (list :chart (list x y lw ch)
          :term (list x (+ y ch) lw (- h ch))
          :browser (list (+ x lw) y (- w lw) h))))

(defun bebop-remote--local-chart (path)
  "The local copy of the host's chart PATH, or nil."
  (when path
    (let ((local (expand-file-name (file-name-nondirectory path)
                                   bebop-charts-dir)))
      (and (file-exists-p local) local))))

(defun bebop-remote--workbench-frame (box chart name)
  "Show CHART for session NAME in the workbench frame, placed at BOX."
  (pcase-let ((`(,x ,y ,w ,h) box))
    (let ((frame (or (seq-find (lambda (f) (frame-parameter f 'bebop-workbench))
                               (frame-list))
                     (make-frame '((name . "Workbench") (bebop-workbench . t))))))
      (set-frame-position frame x y)
      ;; `set-frame-size' sizes the text area; the title bar, fringes
      ;; and scroll bar come on top. Take them off so the outer frame
      ;; fills the box.
      (let ((outer (alist-get 'outer-size (frame-geometry frame))))
        (set-frame-size frame
                        (- w (- (car outer) (frame-text-width frame)))
                        (- h (- (cdr outer) (frame-text-height frame)))
                        t))
      (with-selected-frame frame
        (delete-other-windows)
        (switch-to-buffer
         (if chart
             (find-file-noselect chart)
           (with-current-buffer (get-buffer-create "*bebop-workbench*")
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (format "%s has no chart on this machine.\n" name)))
             (current-buffer)))))
      frame)))

(defun bebop-remote--workbench-term (box name)
  "Pin the W terminal to NAME, opening it at BOX when it does not exist."
  (pcase-let ((`(,x ,y ,w ,h) box)
              (`(,px ,py . ,_) (alist-get 'workarea
                                          (car (display-monitor-attributes-list)))))
    (bebop-remote-steer
     "W" name
     (lambda (ok)
       (unless ok
         (bebop-remote-open-terminal
          "W"
          (format "--window-position-x=%d" (- x px))
          (format "--window-position-y=%d" (- y py))
          (format "--window-width=%d" (/ w (car bebop-remote-term-cell)))
          (format "--window-height=%d" (/ h (cdr bebop-remote-term-cell))))
         (run-at-time 3 nil #'bebop-remote-steer "W" name #'ignore))))))

(defun bebop-remote--dev-url (name)
  "The dev URL for far session NAME: its backline port, else its repo's."
  (let* ((info (ignore-errors
                 (bebop-remote--eval-json
                  (format "(bebop-session-info %S)" name))))
         (ports (plist-get (plist-get info :backline) :ports))
         (port (or (and ports (> (length ports) 0) (aref ports 0))
                   (cdr (assoc (car (split-string name "--")) bebop-remote-dev-ports)))))
    (when port (format "http://localhost:%d/" port))))

(defun bebop-remote--chrome-up-p ()
  "Non-nil when the workbench Chrome answers on its debug port."
  (eq 0 (call-process "curl" nil nil nil "-sf" "-m" "1"
                      (format "http://localhost:%d/json/version"
                              bebop-remote-chrome-debug-port))))

(defun bebop-remote--workbench-browser (box url)
  "Show URL in the workbench Chrome, launching it placed at BOX if needed.
A running one opens URL in a new tab; nothing can move its window."
  (pcase-let ((`(,x ,y ,w ,h) box))
    (if (bebop-remote--chrome-up-p)
        (when url
          (call-process "curl" nil nil nil "-sf" "-m" "2" "-X" "PUT"
                        (format "http://localhost:%d/json/new?%s"
                                bebop-remote-chrome-debug-port url)))
      (apply #'call-process "open" nil 0 nil
             "-na" bebop-remote-chrome-app "--args"
             (format "--user-data-dir=%s"
                     (expand-file-name bebop-remote-chrome-profile))
             (format "--remote-debugging-port=%d" bebop-remote-chrome-debug-port)
             (format "--window-position=%d,%d" x y)
             (format "--window-size=%d,%d" w h)
             "--no-first-run" "--new-window"
             (if url (list url) nil)))))

(defun bebop-remote-workbench-layout (&optional name)
  "Lay out far session NAME's workbench on the second screen.
Chart top left, the W terminal bottom left, Chrome on the right.
Also the `workbench' mailbox verb; returns a short report."
  (interactive)
  (let* ((name (or name
                   (get-text-property (point) 'bebop-remote-name)
                   (user-error "No session at point")))
         (_ (unless (and (stringp name) (not (string-empty-p name)))
              ;; The mailbox verb takes its name from a caller's
              ;; environment; an empty one must not lay anything out.
              (error "bebop-remote-workbench-layout: NAME must be a non-empty string")))
         (s (seq-find (lambda (s) (equal (plist-get s :name) name))
                      bebop-remote--sessions))
         (chart (bebop-remote--local-chart
                 (or (plist-get s :chart)
                     (expand-file-name (concat name ".org") bebop-charts-dir))))
         (boxes (bebop-remote--workbench-boxes))
         (url (bebop-remote--dev-url name)))
    (bebop-remote--workbench-frame (plist-get boxes :chart) chart name)
    (bebop-remote--workbench-term (plist-get boxes :term) name)
    (bebop-remote--workbench-browser (plist-get boxes :browser) url)
    ;; The tunnels: the dev server here, the Chrome debug port there.
    (let ((errs (delq nil
                      (list (when url
                              (when-let ((e (bebop-remote--forward
                                             "-L" (url-port (url-generic-parse-url url)))))
                                (format "dev port: %s" e)))
                            (when-let ((e (bebop-remote--forward
                                           "-R" bebop-remote-chrome-debug-port)))
                              (format "debug port: %s" e))))))
      (message "Bebop: workbench for %s%s%s" name
               (if url (format " at %s" url) "")
               (if errs (concat " — " (string-join errs "; ")) ""))
      (format "workbench: %s %s%s" name (or url "(no dev url)")
              (if errs (concat " — " (string-join errs "; ")) "")))))

(defcustom bebop-remote-client nil
  "Non-nil makes this machine a client of `bebop-remote-host'.
The Conductor shows the host's sessions, and new sessions start there."
  :type 'boolean
  :group 'bebop)

(defun bebop-remote--list (expr)
  "Evaluate EXPR on the host and return its JSON array as a list."
  (append (bebop-remote--eval-json expr) nil))

(defun bebop-remote--org-relative (file)
  "FILE's path relative to `bebop-org-dir', or a user error if outside it.
The host only sees what Dropbox syncs: the org tree."
  (let ((root (file-name-as-directory
               (file-truename (expand-file-name bebop-org-dir))))
        (true (file-truename file)))
    (unless (string-prefix-p root true)
      (user-error "%s is outside %s, so %s cannot see it"
                  file bebop-org-dir bebop-remote-host))
    (file-relative-name true root)))

(defun bebop-remote--read-venue (&optional none)
  "Ask for a venue on the host; return its spec (see `bebop-start-session').
NONE adds a \"none\" choice, which returns nil."
  (pcase (completing-read (format "Venue on %s: " bebop-remote-host)
                          (append '("new" "old" "repo") (when none '("none")))
                          nil t)
    ("new"
     (let* ((repo (completing-read
                   "Repository: "
                   (bebop-remote--list "(bebop-list-repos)") nil t))
            (branch (completing-read
                     "Branch (type a new name to create): "
                     (bebop-remote--list
                      (format "(bebop-list-branches %S)" repo)))))
       (when (string-empty-p (string-trim branch))
         (user-error "Branch name cannot be empty"))
       (list "new" repo branch)))
    ("old" (list "old" (completing-read
                        "Existing venue: "
                        (bebop-remote--list "(bebop-list-venues)") nil t)))
    ("repo" (list "repo" (completing-read
                          "Repo: "
                          (bebop-remote--list "(bebop-list-repos)") nil t)))
    (_ nil)))

(defun bebop-remote--read-chart (&optional none)
  "Ask for a chart; return its spec (see `bebop-start-session').
Offers \"from heading\" and \"use this file\" from an org buffer.
NONE adds a \"none\" choice, which returns nil."
  (let* ((in-org (derived-mode-p 'org-mode))
         (at-heading (and in-org
                          (ignore-errors
                            (save-excursion (org-back-to-heading t) t))))
         (current-file (buffer-file-name)))
    (pcase (completing-read
            "Chart: "
            (append (when at-heading '("from heading"))
                    (when (and in-org current-file) '("use this file"))
                    '("new" "old" "org")
                    (when none '("none")))
            nil t)
      ("from heading" (list "heading" (bebop--subtree-text)))
      ("use this file" (list "file" (bebop-remote--org-relative current-file)))
      ("new" (list "new"))
      ("old" (list "file" (bebop-remote--org-relative
                           (bebop--prompt-existing-chart))))
      ("org" (list "file" (bebop-remote--org-relative
                           (bebop--prompt-any-org))))
      (_ nil))))

(defun bebop-remote-new-session ()
  "Create a session on the host: venue, name, chart, as `bebop-new-session'."
  (interactive)
  (let* ((venue (bebop-remote--read-venue t))
         (default-name (pcase venue
                         (`("new" ,repo ,branch)
                          (format "%s--%s" repo (bebop--sanitize-name branch)))
                         (`(,_ ,dir) dir)))
         (name (read-string "Session name: " default-name))
         (chart (bebop-remote--read-chart t)))
    (bebop-remote--eval-json
     (format "(bebop-start-session %S '%S '%S)" name venue chart))
    (message "Bebop: started %s on %s" name bebop-remote-host)
    (bebop-remote-refresh t)
    (ignore-errors (bebop-remote-select name))))

(defun bebop-remote--name-at-point ()
  "The far session on this row, or a user error."
  (or (get-text-property (point) 'bebop-remote-name)
      (user-error "No session at point")))

(defun bebop-remote--row (name)
  "The last snapshot's row plist for far session NAME."
  (seq-find (lambda (s) (equal (plist-get s :name) name))
            bebop-remote--sessions))

(defun bebop-remote-exile ()
  "Exile the far session at point, after the host's own confirmation text."
  (interactive)
  (let* ((name (bebop-remote--name-at-point))
         (desc (bebop-remote--eval-json (format "(bebop-exile-preview %S)" name))))
    (unless (yes-or-no-p (format "Exile \"%s\" on %s? (%s) "
                                 name bebop-remote-host desc))
      (user-error "Exile cancelled"))
    (bebop-remote--eval-json (format "(bebop-discard-session %S)" name))
    (when (equal name bebop-remote--active)
      (setq bebop-remote--active nil))
    (message "Bebop: exiled %s on %s" name bebop-remote-host)
    (bebop-remote-refresh)))

(defun bebop-remote-associate-chart ()
  "Give the far session at point a chart."
  (interactive)
  (let ((name (bebop-remote--name-at-point)))
    (when (plist-get (bebop-remote--row name) :chart)
      (user-error "%s already has a chart — D dissociates it first" name))
    (bebop-remote--eval-json
     (format "(bebop-set-session-chart %S '%S)" name (bebop-remote--read-chart)))
    (bebop-remote-refresh)))

(defun bebop-remote-dissociate-chart ()
  "Remove the chart from the far session at point (the file stays)."
  (interactive)
  (let ((name (bebop-remote--name-at-point)))
    (bebop-remote--eval-json (format "(bebop-set-session-chart %S nil)" name))
    (message "Bebop: dissociated chart from %s" name)
    (bebop-remote-refresh)))

(defun bebop-remote-associate-venue ()
  "Give the far session at point a venue."
  (interactive)
  (let ((name (bebop-remote--name-at-point)))
    (when (plist-get (bebop-remote--row name) :venue)
      (user-error "%s already has a venue — W dissociates it first" name))
    (bebop-remote--eval-json
     (format "(bebop-set-session-venue %S '%S)" name (bebop-remote--read-venue)))
    (bebop-remote-refresh)))

(defun bebop-remote-dissociate-venue ()
  "Remove the venue from the far session at point (the worktree stays)."
  (interactive)
  (let ((name (bebop-remote--name-at-point)))
    (bebop-remote--eval-json (format "(bebop-set-session-venue %S nil)" name))
    (message "Bebop: dissociated venue from %s" name)
    (bebop-remote-refresh)))

(defun bebop-remote-new-set (set)
  "Declare SET on the host, as N does locally."
  (interactive (list (read-string (format "New set on %s: " bebop-remote-host))))
  (when (string-empty-p (string-trim set))
    (user-error "Set name cannot be empty"))
  (bebop-remote--eval (format "(progn (bebop-new-set %S) t)" set))
  (bebop-remote-refresh))

(defun bebop-remote-assign-set ()
  "Move the far session at point to a set; empty ungroups it."
  (interactive)
  (let* ((name (bebop-remote--name-at-point))
         (set (completing-read (format "Move %s to set (empty to ungroup): " name)
                               (bebop-remote--list "(bebop-list-sets)"))))
    (bebop-remote--eval (format "(progn (bebop-assign-set %S %S) t)" name
                                (unless (string-empty-p (string-trim set)) set)))
    (bebop-remote-refresh)))

(defun bebop-remote--host-tabs ()
  "The dashboard host tabs as (LABEL . BUFFER): this machine, the host.
None on the resident host, which has no far host to show."
  (unless (equal (bebop--host-name) bebop-resident-host)
    (seq-filter (lambda (tab) (buffer-live-p (cdr tab)))
                (list (cons (bebop--host-name) (get-buffer bebop-buffer-name))
                      ;; "Matthews-Pro" → "Pro"
                      (cons (car (last (split-string bebop-remote-presence-host "-")))
                            (get-buffer bebop-remote-buffer-name))))))

(setq bebop-dashboard-tabs-function #'bebop-remote--host-tabs)

;; Headers are buffer-local forms set when the mode starts, so a reload
;; re-arms the live dashboards, and takes down the tab line the first
;; version of host tabs used.
(dolist (b (list bebop-buffer-name bebop-remote-buffer-name))
  (when-let ((buf (get-buffer b)))
    (with-current-buffer buf
      (when (bound-and-true-p tab-line-mode)
        (tab-line-mode -1))
      (cond ((derived-mode-p 'bebop-remote-mode)
             (let ((inhibit-read-only t)) (bebop-remote-mode))
             (bebop-remote--render))
            ((derived-mode-p 'bebop-dashboard-mode)
             (setq header-line-format '((:eval (bebop--dashboard-header)))))))))

(defun bebop-remote--conductor-dashboard (frame)
  "Finish the cold start for FRAME: utility session, D terminal, host tabs.
Every machine gets its bebop utility session and a D tab on its own
tmux; a client also opens on the host's dashboard and gets a D tab
on the host. The resident host has no far host, so it stops there."
  (let ((resident (equal (bebop--host-name) bebop-resident-host)))
    (unless resident
      (let ((win (frame-parameter frame 'bebop-dashboard-window))
            (remote (get-buffer-create bebop-remote-buffer-name)))
        (with-current-buffer remote
          (unless (derived-mode-p 'bebop-remote-mode)
            (bebop-remote-mode)))
        (when (and bebop-remote-client (window-live-p win))
          (set-window-buffer win remote))
        (bebop-remote-refresh)
        (bebop-remote--start-stream)))
    (bebop-remote--ensure-utility-session)
    (bebop-remote--ensure-terminals frame (unless resident
                                            (list (bebop-remote--target))))))

(defcustom bebop-remote-utility-session "bebop"
  "The local utility session a client keeps running, or nil for none."
  :type '(choice (const :tag "None" nil) string)
  :group 'bebop)

(defun bebop-remote--ensure-utility-session ()
  "Start the client's utility session unless it is already running.
The previous selection stays selected."
  (when-let ((name bebop-remote-utility-session))
    (unless (assoc name bebop--live-sessions)
      (let ((active bebop--active-session)
            (chart (expand-file-name (concat name ".org")
                                     (expand-file-name bebop-charts-dir))))
        (bebop--create-session name nil (and (file-exists-p chart) chart))
        (when (and active (assoc active bebop--live-sessions))
          (bebop-select-session active))
        (message "Bebop: started the %s utility session" name)))))

(defun bebop-remote--ensure-terminals (frame &optional far-hosts)
  "Give the D terminal a Ghostty tab for this machine and each of FAR-HOSTS.
Then refocus FRAME. Runs `bebop-term' for each host in turn; an
existing tab is only selected. Skipped where Ghostty or `bebop-term'
is missing."
  (when (and (file-directory-p "/Applications/Ghostty.app")
             (or (executable-find bebop-remote-term-program)
                 (ignore (message "Bebop: %s is not on exec-path; no D terminal"
                                  bebop-remote-term-program))))
    (let ((hosts (cons "local" far-hosts)))
      (cl-labels
          ((next (hosts)
             (if (null hosts)
                 (when (frame-live-p frame)
                   (select-frame-set-input-focus frame))
               (make-process
                :name "bebop-term"
                :command (list bebop-remote-term-program "D" (car hosts))
                :connection-type 'pipe
                :noquery t
                :buffer nil
                :sentinel (lambda (proc _event)
                            (unless (process-live-p proc)
                              (next (cdr hosts))))))))
        (next hosts)))))

(advice-add 'bebop-frame--setup-conductor :after #'bebop-remote--conductor-dashboard)

(defun bebop-remote--remote-context-p ()
  "Non-nil when a new session should start on the host.
The dashboard you are in decides; elsewhere, the host tab the
Conductor's dashboard window shows; with no Conductor, client mode."
  (let* ((conductor (seq-find (lambda (f) (frame-parameter f 'bebop-conductor))
                              (frame-list)))
         (win (and conductor (frame-parameter conductor 'bebop-dashboard-window))))
    (cond ((derived-mode-p 'bebop-remote-mode) t)
          ((derived-mode-p 'bebop-dashboard-mode) nil)
          ((window-live-p win)
           (eq (window-buffer win) (get-buffer bebop-remote-buffer-name)))
          (t bebop-remote-client))))

(defun bebop-remote--new-session-advice (orig &rest args)
  "Start the session on the host when the host tab is the context."
  (if (bebop-remote--remote-context-p)
      (call-interactively #'bebop-remote-new-session)
    (apply orig args)))

(advice-add 'bebop-new-session :around #'bebop-remote--new-session-advice)

(defun bebop-remote-attach ()
  "Attach to the host's agent tmux session — the live-TTY escape hatch.
Uses mosh when installed (survives roaming, IP changes, suspend);
falls back to ssh -t."
  (interactive)
  (require 'term)
  (let* ((target (bebop-remote--target))
         (buf (if (executable-find "mosh")
                  (make-term "bebop-remote-attach" "mosh" nil
                             target "--"
                             bebop-remote-tmux "attach"
                             "-t" bebop-remote-tmux-session)
                (apply #'make-term "bebop-remote-attach" "ssh" nil
                       (append bebop-remote-ssh-options
                               (list "-t" target
                                     (format "%s attach -t %s"
                                             bebop-remote-tmux
                                             (shell-quote-argument
                                              bebop-remote-tmux-session))))))))
    (with-current-buffer buf
      (term-char-mode))
    (switch-to-buffer buf)))

(defun bebop-remote--forward (flag port)
  "Add forward FLAG (\"-L\" or \"-R\") for PORT on the control master.
Remember it on success. Return nil on success, else ssh's message."
  (with-temp-buffer
    (if (eq 0 (apply #'call-process "ssh" nil t nil
                     (append bebop-remote-ssh-options
                             (list "-O" "forward" flag
                                   (format "%d:localhost:%d" port port)
                                   (bebop-remote--target)))))
        (progn (cl-pushnew (cons flag port) bebop-remote--forwards :test #'equal)
               nil)
      (let ((out (string-trim (buffer-string))))
        (if (string-empty-p out) "no control master to ride" out)))))

(defun bebop-remote--reforward ()
  "Set up every remembered forward again, after a reconnect."
  (dolist (f (reverse bebop-remote--forwards))
    (when-let ((err (bebop-remote--forward (car f) (cdr f))))
      (message "Bebop: forward %s %d failed: %s" (car f) (cdr f) err))))

(defun bebop-remote-forward-port (port)
  "Forward local PORT to the same port on the host's localhost, live.
Rides the existing control master (`ssh -O forward'), so no new
connection and no reconnect."
  (interactive "nForward localhost port: ")
  (if-let ((err (bebop-remote--forward "-L" port)))
      (user-error "bebop-remote: localhost:%d not forwarded: %s" port err)
    (message "bebop-remote: localhost:%d → %s:%d" port bebop-remote-host port)))

(provide 'bebop-remote)

;;; bebop-remote.el ends here
