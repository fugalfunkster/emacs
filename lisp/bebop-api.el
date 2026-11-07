;;; bebop-api.el --- JSON API surface for agent citizens -*- lexical-binding: t; -*-

(require 'bebop-session)
(require 'bebop-set)

;; Optional backline integration; soft dependency.
(declare-function bebop--backline-live-p "bebop-backline")
(declare-function bebop-backline-status "bebop-backline")

(defun bebop-session-info (name)
  "Return a JSON description of session NAME for agent orientation.
Fields: session, set, chart, venue, ticket, siblings
\(names sharing the set), backline (live/busy/command/ports, or null)."
  (let* ((row (bebop-set--row name (cdr (assoc name bebop--live-sessions))))
         (set (plist-get row :set))
         (venue (plist-get row :venue))
         (siblings
          (when set
            (vconcat
             (delete name
                     (mapcar (lambda (r) (plist-get r :name))
                             (seq-filter
                              (lambda (r) (equal (plist-get r :set) set))
                              (bebop-set--rows)))))))
         (slug (and venue (file-name-nondirectory
                           (directory-file-name venue))))
         (backline
          (when (and slug
                     (fboundp 'bebop--backline-live-p)
                     (bebop--backline-live-p slug))
            (let ((st (bebop-backline-status slug)))
              (list :live t
                    :busy (if (plist-get st :busy) t :false)
                    :command (or (plist-get st :command) :null)
                    :ports (vconcat (plist-get st :ports))))))
         (mr (bebop-mr--entry name))
         (pipeline (bebop-pipeline--entry name)))
    (json-serialize
     (list :session name
           :set (or set :null)
           :chart (or (plist-get row :chart) :null)
           :venue (or venue :null)
           :ticket (or (bebop--on-deck-ticket-slug name) :null)
           :siblings (or siblings (vector))
           :backline (or backline :null)
           :mr (if mr
                   (list :state (or (plist-get mr :state) :null)
                         :unresolved (or (plist-get mr :unresolved) 0)
                         :iid (or (plist-get mr :iid) :null))
                 :null)
           :pipeline (if pipeline
                         (or (plist-get pipeline :status) :null)
                       :null)))))

(defun bebop-list-sessions ()
  "Return a JSON array of all sessions, running and on deck."
  (json-serialize
   (vconcat
    (mapcar (lambda (r)
              (let ((live (plist-get r :live)))
                (list :name (plist-get r :name)
                      :live (if live t :false)
                      :status (if live
                                  (symbol-name
                                   (or (plist-get live :status) 'unknown))
                                :null)
                      :set (or (plist-get r :set) :null)
                      :chart (or (plist-get r :chart) :null)
                      :venue (or (plist-get r :venue) :null))))
            (bebop-set--rows)))))

(defun bebop-list-sets ()
  "Return a JSON array of all known set names."
  (json-serialize (vconcat (bebop-set--names))))

(defun bebop-resume-session (name)
  "Resume on-deck session NAME without prompts. Returns JSON.
The citizen form of `bebop-resume', for callers with no minibuffer.
It does not change the active session (`bebop--select-on-create')."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "bebop-resume-session: NAME must be a non-empty string"))
  (unless (member name (bebop--on-deck-names))
    (error "bebop-resume-session: %s is not on deck" name))
  (let* ((chart (let ((f (expand-file-name (concat name ".org")
                                           (expand-file-name bebop-charts-dir))))
                  (when (file-exists-p f) f)))
         (venue (let ((d (expand-file-name name (expand-file-name bebop-venues-dir))))
                  (when (file-directory-p d) d)))
         (stored (bebop--session-info name))
         (launcher (if bebop-use-docker 'docker
                     (and stored (plist-get stored :launcher)))))
    (let ((bebop--select-on-create nil))
      (bebop--create-session name venue chart launcher))
    (json-serialize (list :resumed name
                          :chart (or chart :null)
                          :venue (or venue :null)))))

(defun bebop-stop-session (name)
  "Kill live session NAME without prompts; it returns to On deck. JSON.
The citizen form of `bebop-kill-session', for callers with no minibuffer."
  (unless (assoc name bebop--live-sessions)
    (error "bebop-stop-session: %s is not running" name))
  (bebop--reap-runtime name)
  (bebop-refresh)
  (json-serialize (list :stopped name)))

(defun bebop-list-repos ()
  "Return the repositories under `bebop-repos-dir' as a JSON array."
  (json-serialize (vconcat (bebop--list-repos))))

(defun bebop-list-branches (repo)
  "Return REPO's local and remote branches as a JSON array."
  (unless (member repo (bebop--list-repos))
    (error "bebop-list-branches: no repository named %s" repo))
  (json-serialize
   (vconcat (bebop--list-branches
             (expand-file-name repo (expand-file-name bebop-repos-dir))))))

(defun bebop-list-venues ()
  "Return the venue directory names as a JSON array."
  (json-serialize (vconcat (bebop--list-venues))))

(defun bebop--venue-from-spec (venue who)
  "Resolve VENUE spec (see the section text) to a path, or nil.
WHO names the calling verb in errors."
  (pcase venue
    ('nil nil)
    (`("new" ,repo ,branch) (bebop--add-venue repo branch))
    (`("old" ,dir)
     (let ((d (expand-file-name dir (expand-file-name bebop-venues-dir))))
       (unless (member dir (bebop--list-venues))
         (error "%s: no venue named %s" who dir))
       d))
    (`("repo" ,repo)
     (unless (member repo (bebop--list-repos))
       (error "%s: no repository named %s" who repo))
     (expand-file-name repo (expand-file-name bebop-repos-dir)))
    (_ (error "%s: bad venue spec %S" who venue))))

(defun bebop--chart-from-spec (name chart who)
  "Resolve CHART spec for session NAME to a path, or nil.
A new or heading chart is written here. WHO names the verb in errors."
  (pcase chart
    ('nil nil)
    (`("new") (bebop--create-new-chart name))
    (`("heading" ,text)
     (let ((path (bebop--create-new-chart name)))
       (bebop--cue-to-chart text path)
       path))
    (`("file" ,rel)
     (let ((f (expand-file-name rel (expand-file-name bebop-org-dir))))
       (unless (file-exists-p f)
         (error "%s: %s is not here (Dropbox lag?)" who rel))
       f))
    (_ (error "%s: bad chart spec %S" who chart))))

(defun bebop-start-session (name venue chart)
  "Create session NAME from the VENUE and CHART specs, asking nothing.
See the section text for the spec shapes. Returns JSON. The citizen
form of `bebop-new-session', for callers with no minibuffer."
  (unless (and (stringp name) (not (string-empty-p (string-trim name))))
    (error "bebop-start-session: NAME must be a non-empty string"))
  (bebop--assert-name-available name)
  (let* ((venue-path (bebop--venue-from-spec venue "bebop-start-session"))
         (chart-path (bebop--chart-from-spec name chart "bebop-start-session")))
    (bebop--create-session name venue-path chart-path
                           (when bebop-use-docker 'docker))
    (json-serialize (list :started name
                          :venue (or venue-path :null)
                          :chart (or chart-path :null)))))

(defun bebop-exile-preview (name)
  "Describe what exiling NAME discards, as a JSON string."
  (unless (member name (bebop--teardown-names))
    (error "bebop-exile-preview: no session named %s" name))
  (json-serialize (bebop--teardown-desc name)))

(defun bebop-discard-session (name)
  "Exile session NAME without prompts. Returns JSON.
The citizen form of `bebop-exile-session'; confirm before calling."
  (unless (member name (bebop--teardown-names))
    (error "bebop-discard-session: no session named %s" name))
  (bebop-teardown-session name)
  (json-serialize (list :exiled name)))

(defun bebop-set-session-chart (name chart)
  "Associate CHART (a spec) with session NAME; nil dissociates. JSON."
  (if (null chart)
      (bebop-dissociate-chart name)
    (when (bebop--session-chart name)
      (error "%s already has a chart; dissociate it first" name))
    (let ((path (bebop--chart-from-spec name chart "bebop-set-session-chart")))
      (bebop--upsert-session name (list :chart path))
      (bebop--render)))
  (json-serialize (list :session name :chart (or (bebop--session-chart name) :null))))

(defun bebop-set-session-venue (name venue)
  "Associate VENUE (a spec) with session NAME; nil dissociates. JSON.
As locally, a running agent's working directory does not move."
  (if (null venue)
      (bebop-dissociate-venue name)
    (when (bebop--session-venue name)
      (error "%s already has a venue; dissociate it first" name))
    (let ((path (bebop--venue-from-spec venue "bebop-set-session-venue")))
      (bebop--upsert-session name (list :venue path))
      (bebop--render)))
  (json-serialize (list :session name :venue (or (bebop--session-venue name) :null))))

(defconst bebop--snapshot-props
  '(face font-lock-face display help-echo bebop-session-name bebop-on-deck-name
         bebop-fold bebop-mr-item bebop-mr-heading)
  "Text properties a dashboard snapshot keeps. A `help-echo' travels
only when it is a string; a function would not print readably.")

(defun bebop--snapshot-clean (string)
  "Copy STRING keeping only `bebop--snapshot-props'."
  (let ((out (copy-sequence string))
        (len (length string))
        (pos 0))
    (set-text-properties 0 len nil out)
    (while (< pos len)
      (let ((next (next-property-change pos string len))
            (props (text-properties-at pos string)))
        (cl-loop for (k v) on props by #'cddr
                 when (and (memq k bebop--snapshot-props)
                           (or (not (eq k 'help-echo)) (stringp v)))
                 do (put-text-property pos next k v out))
        (setq pos next)))
    out))

(defun bebop-dashboard-snapshot (&optional active folds)
  "Render the setlist tree for a remote client; return it printed.
ACTIVE is the client's selected session; FOLDS an alist of section
value → `hide' or `show'. See the section prose for the reply shape."
  (when-let ((row (and active
                       (seq-find (lambda (r) (equal (plist-get r :name) active))
                                 (bebop-set--rows)))))
    (when (bebop-set--attention-p row)
      (bebop-set-ack-session active)))
  (let* ((rows (bebop-set--rows))
         (body
          (with-temp-buffer
            (magit-section-mode)
            (let ((inhibit-read-only t)
                  (bebop--active-session active)
                  (bebop-hud--pulses nil)
                  (magit-section-set-visibility-hook
                   (list (lambda (section)
                           (cdr (assoc (oref section value) folds))))))
              (bebop-set--render-body)
              (let ((tops (oref magit-root-section children)))
                (dolist (s tops)
                  (when (memq (oref s type) '(bebop-set bebop-ungrouped bebop-derived))
                    (put-text-property (oref s start)
                                       (or (oref s content) (oref s end))
                                       'bebop-fold
                                       (cons (oref s value) (oref s hidden)))))
                ;; Cut folded bodies last-first so earlier positions hold.
                (dolist (s (reverse tops))
                  (when (and (oref s hidden) (oref s content))
                    (delete-region (oref s content) (oref s end)))))
              (bebop--snapshot-clean (buffer-string))))))
    (let ((print-escape-multibyte t)
          (print-escape-nonascii t)
          (print-length nil)
          (print-level nil))
      (prin1-to-string
       (list :body body
             :pulse (bebop--pulse-health)
             :rows (mapcar
                    (lambda (r)
                      (let ((live (plist-get r :live)))
                        (list :name (plist-get r :name)
                              :live (and live t)
                              :status (and live
                                           (symbol-name
                                            (or (plist-get live :status) 'unknown)))
                              :attention (and (bebop-set--attention-p r) t)
                              :set (plist-get r :set)
                              :chart (plist-get r :chart)
                              :venue (plist-get r :venue))))
                    rows))))))

(defun bebop-propose-exile (name)
  "Queue session NAME for exile approval. Returns the queue as JSON."
  (unless (member name bebop--exile-proposals)
    (push name bebop--exile-proposals)
    (bebop-set--save))
  (bebop-exile-proposals))

(defun bebop-exile-proposals ()
  "Return the exile-proposal queue as a JSON array."
  (json-serialize (vconcat bebop--exile-proposals)))

(defun bebop-clear-exile-proposal (name)
  "Remove NAME from the exile-proposal queue. Returns the queue as JSON."
  (setq bebop--exile-proposals (delete name bebop--exile-proposals))
  (bebop-set--save)
  (bebop-exile-proposals))

(defun bebop-prune-empty-sets ()
  "Remove every set with no members. Returns the removed names as JSON."
  (json-serialize (vconcat (bebop-set-prune-empty))))

(declare-function bebop-backline-sweep "bebop-backline")

(defun bebop-sweep-backlines ()
  "Close idle orphan backlines. Return [{slug, outcome}] as JSON."
  (json-serialize
   (vconcat (when (fboundp 'bebop-backline-sweep)
              (mapcar (lambda (r)
                        (list :slug (car r) :outcome (symbol-name (cdr r))))
                      (bebop-backline-sweep))))))

(provide 'bebop-api)

;;; bebop-api.el ends here
