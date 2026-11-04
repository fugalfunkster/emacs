;;; bebop-remote.el --- Live Remote: drive a resident host's bebop over ssh -*- lexical-binding: t; -*-

(require 'seq)
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

(defvar bebop-remote--sessions nil
  "Last fetched session list (a list of plists), or nil.")

(defvar bebop-remote--error nil
  "Error string from the last failed refresh, or nil when reachable.")

(defvar bebop-remote--timer nil)

(defun bebop-remote--insert-row (s)
  "Insert one dashboard line for session plist S."
  (let* ((name (plist-get s :name))
         (live (plist-get s :live))
         (status (bebop-remote--status s))
         (set (plist-get s :set))
         (dot-face (cond ((not live) 'shadow)
                         ((member status '("blocked" "waiting"))
                          'bebop-dot-waiting-face)
                         ((member status '("unknown" "degraded" "gone"))
                          'bebop-dot-degraded-face)
                         (t 'bebop-dot-active-face)))
         (start (point)))
    (insert (propertize (if live "●" "○") 'face dot-face))
    (insert " ")
    (insert (propertize name 'face (if live 'bebop-session-face 'shadow)))
    (when set
      (insert (propertize (format "  [%s]" set) 'face 'shadow)))
    (insert "\n")
    (add-text-properties start (point) (list 'bebop-remote-name name))
    (when live
      (when-let ((children (plist-get (car (bebop-remote--hook name)) :children)))
        (bebop--insert-subagent-rows name "  " children)))))

(defun bebop-remote--render ()
  "Redraw the remote dashboard from the last fetched state."
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
         ((null bebop-remote--sessions)
          (insert (propertize "  No sessions on the host.\n" 'face 'shadow)))
         (t
          (dolist (s (seq-filter (lambda (s) (plist-get s :live))
                                 bebop-remote--sessions))
            (bebop-remote--insert-row s))
          (let ((deck (seq-remove (lambda (s) (plist-get s :live))
                                  bebop-remote--sessions)))
            (when deck
              (insert (propertize "\n  On deck\n" 'face 'shadow))
              (dolist (s deck)
                (bebop-remote--insert-row s))))))
        (insert "\n")
        (insert (propertize
                 "g: refresh  RET: compose  a: attach (mosh)  P: forward port\n"
                 'face '(:inherit shadow :height 0.75)))
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

(defun bebop-remote-refresh ()
  "Fetch the host's session pool and re-render; restart polling on success."
  (interactive)
  (condition-case err
      (progn
        (setq bebop-remote--sessions
              (append (bebop-remote--eval-json "(bebop-list-sessions)") nil)
              bebop-remote--error nil)
        (bebop-remote--start-timer))
    (error
     (setq bebop-remote--error (error-message-string err))
     (bebop-remote--stop-timer)))
  (bebop-remote--render))

(defun bebop-remote--poll ()
  (let ((buf (get-buffer bebop-remote-buffer-name)))
    (if (buffer-live-p buf)
        (when (get-buffer-window buf t)
          (bebop-remote-refresh))
      (bebop-remote--stop-timer))))

(defun bebop-remote--start-timer ()
  (unless (timerp bebop-remote--timer)
    (setq bebop-remote--timer
          (run-at-time bebop-remote-poll-interval
                       bebop-remote-poll-interval
                       #'bebop-remote--poll))))

(defun bebop-remote--stop-timer ()
  (when (timerp bebop-remote--timer)
    (cancel-timer bebop-remote--timer))
  (setq bebop-remote--timer nil))

(defun bebop-remote--next ()
  "Move point to the next session line."
  (interactive)
  (let ((start (point)))
    (when (get-text-property (point) 'bebop-remote-name)
      (forward-line 1))
    (while (and (< (point) (point-max))
                (null (get-text-property (point) 'bebop-remote-name)))
      (forward-line 1))
    (unless (get-text-property (point) 'bebop-remote-name)
      (goto-char start))))

(defun bebop-remote--prev ()
  "Move point to the previous session line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (> (point) (point-min))
                (null (get-text-property (point) 'bebop-remote-name)))
      (forward-line -1))
    (unless (get-text-property (point) 'bebop-remote-name)
      (goto-char start))))

(defvar bebop-remote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")      #'bebop-remote-refresh)
    (define-key map (kbd "RET")    #'bebop-remote-compose)
    (define-key map (kbd "a")      #'bebop-remote-attach)
    (define-key map (kbd "P")      #'bebop-remote-forward-port)
    (define-key map (kbd "j")      #'bebop-remote--next)
    (define-key map (kbd "p")      #'bebop-remote--prev)
    (define-key map (kbd "<down>") #'bebop-remote--next)
    (define-key map (kbd "<up>")   #'bebop-remote--prev)
    map)
  "Keymap for `bebop-remote-mode'.")

(define-derived-mode bebop-remote-mode special-mode "BebopRemote"
  "Dashboard for a resident host's session pool, rendered over ssh.
\\{bebop-remote-mode-map}"
  (setq truncate-lines t)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (setq header-line-format
        '((:eval
           (concat
            (propertize (format " Remote: %s" bebop-remote-host)
                        'face `(:height 1.6 :weight bold
                                :foreground ,bebop-header-color))
            (unless (process-live-p bebop-remote--stream)
              (propertize "  stream down" 'face 'shadow))))))
  (add-hook 'kill-buffer-hook #'bebop-remote--stop-timer nil t)
  (add-hook 'kill-buffer-hook #'bebop-remote--stop-stream nil t))

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
         (cmd (format "%s load-buffer - && %s paste-buffer -d -t %s && %s send-keys -t %s C-m"
                      bebop-remote-tmux bebop-remote-tmux qt
                      bebop-remote-tmux qt))
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

(defun bebop-remote-compose (&optional name)
  "Open the composition buffer for the far session NAME (or at point)."
  (interactive)
  (let* ((name (or name
                   (get-text-property (point) 'bebop-remote-name)
                   (user-error "No session at point")))
         (buf (get-buffer-create (format "*bebop-remote: %s*" name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (when (fboundp 'bebop-composition-mode)
        (bebop-composition-mode 1))
      (bebop--apply-buffer-settings)
      (setq bebop-remote--compose-session name)
      (bebop-remote-compose-mode 1)
      (bebop--set-header-overlay (format "Remote → %s" name)
                                 bebop-session-header-color 2.0))
    (pop-to-buffer buf)
    (message "Compose, then C-c C-c to send to %s on %s"
             name bebop-remote-host)))

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

(defun bebop-remote--stream-script ()
  "The stream program's text, from this machine's own tangled copy."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "bin/bebop-event-stream" user-emacs-directory))
    (buffer-string)))

(defun bebop-remote--stream-filter (proc output)
  "Fold each whole line of stream OUTPUT; redraw once the burst ends."
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
          (run-at-time 0.2 nil #'bebop-remote--render))))

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
    ;; A replay rebuilds every state, so start from nothing.
    (clrhash bebop-remote--hook-states)
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
      (setq bebop-remote--stream proc))))

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

(defun bebop-remote--status (s)
  "The status string to draw for far session plist S.
The stream wins: it is newer than the host's poll. One exception, as on
the host: no hook fires on an Esc interrupt, so a session the events
still call `working' is idle when the host's pane check says so and no
event came for a while."
  (let* ((hook (bebop-remote--hook (plist-get s :name)))
         (eff (car hook))
         (polled (plist-get s :status)))
    (cond
     ((null eff) polled)
     ((and (equal (plist-get eff :state) "working")
           (equal polled "waiting")
           (> (- (float-time) (or (cdr hook) 0)) 10))
      polled)
     (t (pcase (plist-get eff :state)
          ("working" "active")
          ((or "blocked" "error") "blocked")
          (_ "waiting"))))))

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

(defun bebop-remote-forward-port (port)
  "Forward local PORT to the same port on the host's localhost, live.
Rides the existing control master (`ssh -O forward'), so no new
connection and no reconnect."
  (interactive "nForward localhost port: ")
  (let ((spec (format "%d:localhost:%d" port port)))
    (if (eq 0 (apply #'call-process "ssh" nil nil nil
                     (append bebop-remote-ssh-options
                             (list "-O" "forward" "-L" spec
                                   (bebop-remote--target)))))
        (message "bebop-remote: localhost:%d → %s:%d"
                 port bebop-remote-host port)
      (user-error "bebop-remote: no control master to ride — refresh the dashboard first"))))

(provide 'bebop-remote)

;;; bebop-remote.el ends here
