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

(defconst bebop-call-verbs
  '(("list-sessions"   . bebop-list-sessions)
    ("list-sets"       . bebop-list-sets)
    ("session-info"    . bebop-session-info)
    ("exile-proposals" . bebop-exile-proposals)
    ("jam"             . bebop-jam))
  "Verb allowlist for incoming calls.
Never dispatch arbitrary elisp from the mailbox: the call file is
Dropbox-writable, and the allowlist is what keeps that acceptable.")

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
          (append (bebop-call--read-json (bebop-call--call-file)) nil))))

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
