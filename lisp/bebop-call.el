;;; bebop-call.el --- Call and response: cross-machine mailbox over Dropbox -*- lexical-binding: t; -*-

(require 'bebop-session)
(require 'bebop-api)
(require 'seq)

(defun bebop-call--default-dir ()
  "Return the mailbox directory under the Dropbox root, or nil.
Checks the macOS CloudStorage location first, then the classic path
(the other machine mounts Dropbox at ~/Dropbox)."
  (when-let ((root (seq-find (lambda (d) (file-directory-p (expand-file-name d)))
                             '("~/Library/CloudStorage/Dropbox" "~/Dropbox"))))
    (expand-file-name ".bebop" (expand-file-name root))))

(defcustom bebop-call-dir (bebop-call--default-dir)
  "Mailbox directory shared between machines (inside Dropbox).
nil disables call-and-response on this machine."
  :type '(choice (const nil) directory)
  :group 'bebop)

(defcustom bebop-call-host (car (split-string (system-name) "\\."))
  "This machine's name in the mailbox protocol (short hostname)."
  :type 'string
  :group 'bebop)

(defcustom bebop-call-poll-interval 15
  "Seconds between mailbox polls."
  :type 'integer
  :group 'bebop)

(defcustom bebop-presence-interval 300
  "Seconds between presence heartbeat writes (see `* Two-machine protocol')."
  :type 'integer
  :group 'bebop)

(defconst bebop-call-verbs
  '(("list-sessions"   . bebop-list-sessions)
    ("list-sets"       . bebop-list-sets)
    ("session-info"    . bebop-session-info)
    ("exile-proposals" . bebop-exile-proposals)
    ("jam"             . bebop-jam)
    ("relay-done"      . bebop-relay-done))
  "Verb allowlist for incoming calls.
Never dispatch arbitrary elisp from the mailbox: the call file is
Dropbox-writable, and the allowlist is what keeps that acceptable.
`relay-done' stays within that stance: it only stages a proposal in
this machine's own ledger; a human executes it at a roundup checkpoint.")

(defvar bebop-call--timer nil)

(defvar bebop-call--seen nil
  "Call ids already answered; seeded from the response file on start.")

(defun bebop-call--call-file ()
  (expand-file-name (format "%s.call.json" bebop-call-host) bebop-call-dir))

(defun bebop-call--response-file ()
  (expand-file-name (format "%s.response.json" bebop-call-host) bebop-call-dir))

(defun bebop-call--read-json (file)
  "Parse FILE as JSON, or nil if missing or momentarily malformed.
Dropbox writes are atomic-rename so torn reads are unlikely, but a
malformed read is retried on the next poll rather than erroring."
  (when (file-exists-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-buffer))
      (error nil))))

(defun bebop-call--seed-seen ()
  (setq bebop-call--seen
        (mapcar (lambda (r) (gethash "id" r))
                (append (bebop-call--read-json (bebop-call--response-file))
                        nil))))

(defun bebop-call--respond (id ok value)
  "Append a response for call ID to our response file (keep last 50)."
  (push id bebop-call--seen)
  (let* ((file (bebop-call--response-file))
         (existing (append (bebop-call--read-json file) nil))
         (entry (let ((h (make-hash-table :test #'equal)))
                  (puthash "id" id h)
                  (puthash "ok" ok h)
                  (puthash "value" (or value :null) h)
                  (puthash "at" (bebop--now-string) h)
                  h))
         (all (append existing (list entry))))
    (when (> (length all) 50)
      (setq all (nthcdr (- (length all) 50) all)))
    (with-temp-file file
      (insert (json-serialize (vconcat all))))))

(defun bebop-call--dispatch (call)
  "Answer CALL (a parsed hash table) unless already seen."
  (let* ((id (gethash "id" call))
         (verb (gethash "verb" call))
         (args (append (gethash "args" call) nil))
         (fn (cdr (assoc verb bebop-call-verbs))))
    (when (and id (not (member id bebop-call--seen)))
      (cond
       ((null fn)
        (bebop-call--respond id :false (format "unknown verb: %s" verb)))
       (t
        (condition-case err
            (bebop-call--respond id t (apply fn args))
          (error
           (bebop-call--respond id :false (error-message-string err)))))))))

(defun bebop-call--poll ()
  (when (and bebop-call-dir (file-directory-p bebop-call-dir))
    (mapc #'bebop-call--dispatch
          (append (bebop-call--read-json (bebop-call--call-file)) nil))
    (bebop-call--maybe-presence)))

(defvar bebop-call--presence-at nil
  "Time of the last presence write, or nil before the first.")

(defun bebop-call--presence-file (&optional host)
  (expand-file-name (format "%s.presence.json" (or host bebop-call-host))
                    bebop-call-dir))

(defun bebop-call--write-presence ()
  "Write this host's presence heartbeat to the mailbox."
  (let* ((rows (bebop-set--rows))
         (live (seq-filter (lambda (r) (plist-get r :live)) rows)))
    (with-temp-file (bebop-call--presence-file)
      (insert (json-serialize
               (list :host bebop-call-host
                     :at (bebop--now-string)
                     :live (length live)
                     :sessions
                     (vconcat
                      (mapcar (lambda (r)
                                (list :name (plist-get r :name)
                                      :status (symbol-name
                                               (or (plist-get (plist-get r :live) :status)
                                                   'unknown))))
                              live))))))
    (setq bebop-call--presence-at (current-time))))

(defun bebop-call--maybe-presence ()
  (when (or (null bebop-call--presence-at)
            (>= (float-time (time-subtract (current-time) bebop-call--presence-at))
                bebop-presence-interval))
    (bebop-call--write-presence)))

(defun bebop-ledger--file (&optional host)
  (expand-file-name (format "%s.proposals.json" (or host bebop-call-host))
                    bebop-call-dir))

(defun bebop-ledger--entries (&optional host)
  (append (bebop-call--read-json (bebop-ledger--file host)) nil))

(defun bebop-ledger--write (entries)
  "Write ENTRIES to this host's own ledger, keeping the last 100."
  (when (> (length entries) 100)
    (setq entries (nthcdr (- (length entries) 100) entries)))
  (with-temp-file (bebop-ledger--file)
    (insert (json-serialize (vconcat entries)))))

(defun bebop-ledger-read (&optional host)
  "Return HOST's ledger (default: this host) as a JSON string."
  (json-serialize (vconcat (bebop-ledger--entries host))))

(defun bebop-ledger-add (kind session &optional detail)
  "Queue a pending KIND proposal for SESSION in this host's ledger.
Idempotent while a matching proposal is pending: re-adding refreshes
DETAIL and the timestamp instead of duplicating. Returns the ledger as
JSON."
  (let* ((entries (bebop-ledger--entries))
         (dup (seq-find (lambda (e)
                          (and (equal (gethash "kind" e) kind)
                               (equal (gethash "session" e) session)
                               (equal (gethash "status" e) "pending")))
                        entries)))
    (if dup
        (progn
          (when detail (puthash "detail" detail dup))
          (puthash "updated-at" (bebop--now-string) dup))
      (let ((h (make-hash-table :test #'equal)))
        (puthash "id" (format "%s-%s-%s-%s" bebop-call-host kind session
                              (format-time-string "%s"))
                 h)
        (puthash "kind" kind h)
        (puthash "session" session h)
        (puthash "detail" (or detail :null) h)
        (puthash "status" "pending" h)
        (puthash "proposed-at" (bebop--now-string) h)
        (puthash "updated-at" (bebop--now-string) h)
        (setq entries (append entries (list h)))))
    (bebop-ledger--write entries)
    (bebop-ledger-read)))

(defun bebop-ledger-set-status (id status)
  "Set proposal ID in this host's ledger to STATUS. Returns the ledger as JSON."
  (let ((entries (bebop-ledger--entries)))
    (dolist (e entries)
      (when (equal (gethash "id" e) id)
        (puthash "status" status e)
        (puthash "updated-at" (bebop--now-string) e)))
    (bebop-ledger--write entries)
    (bebop-ledger-read)))

(defun bebop-relay-done (session to-host)
  "Record that SESSION was relayed away to TO-HOST.
Stages a retire proposal in this host's own ledger for the next roundup
checkpoint; executes nothing."
  (bebop-ledger-add "retire" session (format "relayed to %s" to-host)))

(define-minor-mode bebop-call-mode
  "Answer cross-machine bebop calls from the Dropbox mailbox."
  :global t
  :group 'bebop
  (if bebop-call-mode
      (progn
        (unless bebop-call-dir
          (setq bebop-call-mode nil)
          (user-error "bebop-call: no Dropbox directory found"))
        (make-directory bebop-call-dir t)
        (bebop-call--seed-seen)
        (bebop-call--write-presence)
        (setq bebop-call--timer
              (run-at-time bebop-call-poll-interval
                           bebop-call-poll-interval
                           #'bebop-call--poll))
        (message "bebop-call: answering as %s" bebop-call-host))
    (when (timerp bebop-call--timer)
      (cancel-timer bebop-call--timer))
    (setq bebop-call--timer nil)))

(provide 'bebop-call)

;;; bebop-call.el ends here
