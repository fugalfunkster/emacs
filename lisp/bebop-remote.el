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
         (dot-face (if (and live (bebop-remote--unseen-p name status))
                       (list '(:weight bold) dot-face)
                     dot-face))
         (start (point)))
    (insert (propertize (if live "●" "○") 'face dot-face))
    (insert " ")
    (insert (propertize name 'face (cond ((not live) 'shadow)
                                         ((equal name bebop-remote--active)
                                          'bebop-selected-face)
                                         (t 'bebop-session-face))))
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
          (bebop-remote--notice-edges)
          ;; Unseen attention goes first; the rest keep the host's order.
          (let ((live (seq-filter (lambda (s) (plist-get s :live))
                                  bebop-remote--sessions)))
            (dolist (s (append (seq-filter #'bebop-remote--attention-p live)
                               (seq-remove #'bebop-remote--attention-p live)))
              (bebop-remote--insert-row s)))
          (let ((deck (seq-remove (lambda (s) (plist-get s :live))
                                  bebop-remote--sessions)))
            (when deck
              (insert (propertize "\n  On deck\n" 'face 'shadow))
              (dolist (s deck)
                (bebop-remote--insert-row s))))))
        (insert "\n")
        (insert (propertize
                 "g: refresh  RET: select  r: resume  w: pin W  W: workbench  a: attach (mosh)  P: forward port  .: mark seen\n"
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
    (define-key map (kbd "RET")    #'bebop-remote-select)
    (define-key map (kbd "a")      #'bebop-remote-attach)
    (define-key map (kbd "P")      #'bebop-remote-forward-port)
    (define-key map (kbd ".")      #'bebop-remote-mark-all-seen)
    (define-key map (kbd "w")      #'bebop-remote-workbench)
    (define-key map (kbd "W")      #'bebop-remote-workbench-layout)
    (define-key map (kbd "r")      #'bebop-remote-resume)
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
              (propertize "  stream down" 'face 'shadow))
            (when bebop-remote--forwards
              (propertize
               (concat "  ⇄ "
                       (mapconcat (lambda (f)
                                    (format "%s%d" (if (equal (car f) "-R") "↑" "")
                                            (cdr f)))
                                  (reverse bebop-remote--forwards) " "))
               'face 'shadow))))))
  (add-hook 'kill-buffer-hook #'bebop-remote--stop-timer nil t)
  (add-hook 'kill-buffer-hook #'bebop-remote--stop-stream nil t)
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
      (bebop--set-header-overlay (format "Remote → %s" name)
                                 bebop-session-header-color 2.0))
    buf))

(defun bebop-remote-select (&optional name)
  "Select far session NAME (or the one at point), as RET does locally.
Its composition buffer takes the Conductor's composition window (or
another window when there is no Conductor), the D terminal switches to
it, and it counts as seen. Moving point alone changes nothing."
  (interactive)
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
    (bebop-remote-steer "D" name)
    (bebop-remote-ack name)
    (bebop-remote--render)
    (message "Bebop: %s on %s — compose, then C-c C-c to send"
             name bebop-remote-host)))

(defun bebop-remote-compose (&optional name)
  "Open far session NAME's composition buffer in the selected window."
  (interactive)
  (let ((name (or name
                  (get-text-property (point) 'bebop-remote-name)
                  (user-error "No session at point"))))
    (pop-to-buffer (bebop-remote--compose-buffer name))
    (bebop-remote-ack name)
    (when (get-buffer bebop-remote-buffer-name)
      (bebop-remote--render))))

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

(defcustom bebop-remote-acks-file "~/.emacs.d-local/bebop-remote-acks.eld"
  "Where this machine keeps its seen marks for the host's sessions."
  :type 'file
  :group 'bebop)

(defvar bebop-remote--acks nil
  "Alist of far session name → status string as of your last look.
Loaded from `bebop-remote-acks-file' on first use.")

(defvar bebop-remote--acks-loaded nil)

(defvar bebop-remote--seen-status nil
  "Alist of far session name → the status last drawn, for edge detection.")

(defvar bebop-remote--periphery nil
  "Cached mode-line segment for far attention, or nil when there is none.")

(defun bebop-remote--load-acks ()
  "Read the seen marks once per Emacs."
  (unless bebop-remote--acks-loaded
    (setq bebop-remote--acks-loaded t
          bebop-remote--acks
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents (expand-file-name bebop-remote-acks-file))
              (read (current-buffer)))))))

(defun bebop-remote--save-acks ()
  "Write the seen marks."
  (let ((file (expand-file-name bebop-remote-acks-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (prin1 bebop-remote--acks (current-buffer)))))

(defun bebop-remote-ack (name &optional no-save)
  "Mark far session NAME seen as of the status last drawn.
NO-SAVE skips the write, for callers that ack many and save once."
  (bebop-remote--load-acks)
  (when-let ((status (alist-get name bebop-remote--seen-status nil nil #'equal)))
    (setf (alist-get name bebop-remote--acks nil nil #'equal) status)
    (unless no-save (bebop-remote--save-acks))))

(defun bebop-remote-mark-all-seen ()
  "Mark every far session seen."
  (interactive)
  (dolist (c bebop-remote--seen-status)
    (bebop-remote-ack (car c) t))
  (bebop-remote--save-acks)
  (bebop-remote--render)
  (message "Bebop: all %s sessions marked seen" bebop-remote-host))

(defun bebop-remote--unseen-p (name status)
  "Non-nil if far session NAME's STATUS differs from its seen mark."
  (bebop-remote--load-acks)
  (when-let ((acked (alist-get name bebop-remote--acks nil nil #'equal)))
    (not (equal acked status))))

(defun bebop-remote--attention-p (s)
  "Non-nil if far session plist S waits for you and you have not looked."
  (let ((status (bebop-remote--status s)))
    (and (plist-get s :live)
         (member status '("waiting" "blocked"))
         (bebop-remote--unseen-p (plist-get s :name) status))))

(defun bebop-remote--watching-p (name)
  "Non-nil if far session NAME is the one you selected (RET)."
  (equal name bebop-remote--active))

(defun bebop-remote--notice-edges ()
  "Alert on far sessions that started to wait; refresh the mode line.
Called before each render, so every source of a new status — the
stream or the poll — passes through here."
  (let ((quiet (< (float-time) bebop-remote--quiet-until)))
    (dolist (s bebop-remote--sessions)
      (when (plist-get s :live)
        (let* ((name (plist-get s :name))
               (status (bebop-remote--status s))
               (before (alist-get name bebop-remote--seen-status nil nil #'equal)))
          (setf (alist-get name bebop-remote--seen-status nil nil #'equal) status)
          (when (and before (not (equal before status)))
            (if (bebop-remote--watching-p name)
                (bebop-remote-ack name)
              (when (and (not quiet) (member status '("waiting" "blocked")))
                (let ((msg (plist-get (car (bebop-remote--hook name)) :msg)))
                  (bebop-alert)
                  (message "Bebop: %s on %s is %s%s" name bebop-remote-host
                           (if (equal status "blocked") "blocked" "waiting")
                           (if msg (format " — %s" msg) ""))))))))))
  (let ((n (seq-count #'bebop-remote--attention-p bebop-remote--sessions)))
    (setq bebop-remote--periphery
          (when (> n 0)
            (concat " " (propertize (format "%s ● %d"
                                            ;; "Matthews-Pro" → "Pro"
                                            (car (last (split-string
                                                        bebop-remote-presence-host
                                                        "-")))
                                            n)
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
                         (setq bebop-remote--missing-roles
                               (delete role bebop-remote--missing-roles))
                       (unless (or callback
                                   (member role bebop-remote--missing-roles))
                         (push role bebop-remote--missing-roles)
                         (message "Bebop: no %s terminal for %s — run %s %s"
                                  role name bebop-remote-term-program role)))
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
