;;; bebop-set.el --- Sets and the setlist dashboard tree -*- lexical-binding: t; -*-

(require 'bebop-core)
(require 'bebop-dashboard)
(require 'bebop-session)
(require 'magit-section)
(require 'seq)

;; Optional backline integration (port annotations); soft dependency.
(declare-function bebop--backline-slugs "bebop-backline")
(declare-function bebop--backline-ports "bebop-backline")

(defcustom bebop-set-state-file (locate-user-emacs-file "bebop-state.json")
  "File persisting set declarations, set membership, and exile proposals."
  :type 'file
  :group 'bebop)

(defvar bebop--sets nil
  "Alist of (SET-NAME . PLIST) for declared sets.
PLIST keys: :created-at.  Chart/repo/session attributes arrive lazily
in later phases; a set is at minimum a name.")

(defvar bebop--session-meta nil
  "Alist of (SESSION-NAME . PLIST) persisting per-session choices.
PLIST keys: :set (string or nil), :last-activity (ISO timestamp
string, stamped by the HUD's timekeeping), :acked-status and
:acked-mr (the seen-unseen ack snapshot — glyph states as of the
last visit).
Keyed by name, independent of liveness — applies equally to running
sessions and on-deck artifacts.")

(defvar bebop--exile-proposals nil
  "List of session names queued for exile approval.
Populated by roundup (via `bebop-propose-exile' in bebop-api);
consumed interactively. Persisted with the other choices.")

(defun bebop-set--save ()
  "Persist sets and session choices to `bebop-set-state-file'."
  (let ((sets (make-hash-table :test #'equal))
        (sessions (make-hash-table :test #'equal))
        (doc (make-hash-table :test #'equal)))
    (dolist (cell bebop--sets)
      (let ((attrs (make-hash-table :test #'equal)))
        (when-let ((c (plist-get (cdr cell) :created-at)))
          (puthash "created-at" c attrs))
        (puthash (car cell) attrs sets)))
    (dolist (cell bebop--session-meta)
      (let ((entry (make-hash-table :test #'equal)))
        (when-let ((s (plist-get (cdr cell) :set)))
          (puthash "set" s entry))
        (when-let ((a (plist-get (cdr cell) :last-activity)))
          (puthash "last-activity" a entry))
        (when-let ((a (plist-get (cdr cell) :acked-status)))
          (puthash "acked-status" a entry))
        (when-let ((a (plist-get (cdr cell) :acked-mr)))
          (puthash "acked-mr" a entry))
        (when (> (hash-table-count entry) 0)
          (puthash (car cell) entry sessions))))
    (puthash "version" 2 doc)
    (puthash "sets" sets doc)
    (puthash "sessions" sessions doc)
    (when bebop--exile-proposals
      (puthash "proposals" (vconcat bebop--exile-proposals) doc))
    (with-temp-file bebop-set-state-file
      (insert (json-serialize doc)))))

(defun bebop-set--load ()
  "Load sets and session choices from `bebop-set-state-file'.
Silently ignores missing files and pre-version-2 documents (the v1
format was written by a retired implementation and never read)."
  (when (file-exists-p bebop-set-state-file)
    (condition-case err
        (let* ((doc (with-temp-buffer
                      (insert-file-contents bebop-set-state-file)
                      (json-parse-buffer)))
               (version (and (hash-table-p doc) (gethash "version" doc))))
          (when (and (numberp version) (>= version 2))
            (setq bebop--sets nil bebop--session-meta nil)
            (when-let ((sets (gethash "sets" doc)))
              (maphash (lambda (name attrs)
                         (push (cons name
                                     (when-let ((c (gethash "created-at" attrs)))
                                       (list :created-at c)))
                               bebop--sets))
                       sets))
            (when-let ((sessions (gethash "sessions" doc)))
              (maphash (lambda (name entry)
                         (push (cons name
                                     (append
                                      (when-let ((s (gethash "set" entry)))
                                        (list :set s))
                                      (when-let ((a (gethash "last-activity" entry)))
                                        (list :last-activity a))
                                      (when-let ((a (gethash "acked-status" entry)))
                                        (list :acked-status a))
                                      (when-let ((a (gethash "acked-mr" entry)))
                                        (list :acked-mr a))))
                               bebop--session-meta))
                       sessions))
            (setq bebop--sets (nreverse bebop--sets)
                  bebop--session-meta (nreverse bebop--session-meta))
            (setq bebop--exile-proposals
                  (append (gethash "proposals" doc) nil))))
      (error (message "bebop-set: could not read %s (%s)"
                      bebop-set-state-file (error-message-string err))))))

(defun bebop-set--session-meta (name)
  "Return the persisted choice plist for session NAME."
  (cdr (assoc name bebop--session-meta)))

(defun bebop-set--session-set (name)
  "Return the set NAME belongs to, or nil if ungrouped."
  (plist-get (bebop-set--session-meta name) :set))

(defun bebop-set--update-session-meta (name &rest props)
  "Merge PROPS into session NAME's choice plist and persist."
  (let ((cell (assoc name bebop--session-meta)))
    (unless cell
      (setq cell (cons name nil))
      (push cell bebop--session-meta))
    (let ((plist (cdr cell)))
      (while props
        (setq plist (plist-put plist (car props) (cadr props)))
        (setq props (cddr props)))
      (setcdr cell plist)))
  (bebop-set--save))

(defun bebop-set--declare (name)
  "Declare set NAME if not already declared. Return NAME."
  (unless (assoc name bebop--sets)
    (push (cons name (list :created-at (bebop--now-string))) bebop--sets)
    (bebop-set--save))
  name)

(defun bebop-set--names ()
  "Return all known set names: declared plus referenced."
  (seq-uniq
   (append (mapcar #'car bebop--sets)
           (delq nil (mapcar (lambda (c) (plist-get (cdr c) :set))
                             bebop--session-meta)))))

(defcustom bebop-hud-quiet-hours 4
  "Hours without activity after which a live row's name dims to shadow.
The dot keeps its status color; only the name quiets. Row brightness
tracks recency of attention, not mere liveness."
  :type 'number
  :group 'bebop)

(defun bebop-set--stamp-activity (name _event)
  "Stamp NAME's :last-activity with now. Hook target.
The active session also self-acks: its transitions happen in front of
you, so they never read as unseen (see Seen-unseen below)."
  (bebop-set--update-session-meta name :last-activity (bebop--now-string))
  (when (equal name bebop--active-session)
    (bebop-set-ack-session name)))

(add-hook 'bebop-session-activity-functions #'bebop-set--stamp-activity)

(defun bebop-set--last-activity (name)
  "Return NAME's persisted :last-activity ISO string, or nil."
  (plist-get (bebop-set--session-meta name) :last-activity))

(defun bebop-set--age-seconds (iso)
  "Return seconds elapsed since ISO timestamp, or nil if unparseable."
  (when iso
    (condition-case nil
        (max 0 (- (float-time) (float-time (date-to-time iso))))
      (error nil))))

(defun bebop-set--age-string (iso)
  "Render ISO timestamp's age compactly: \"5m\", \"2h\", \"3d\".
Nil when ISO is nil or unparseable — no stamp, no column."
  (when-let ((secs (bebop-set--age-seconds iso)))
    (cond
     ((< secs 3600)  (format "%dm" (floor secs 60)))
     ((< secs 86400) (format "%dh" (floor secs 3600)))
     (t              (format "%dd" (floor secs 86400))))))

(defun bebop-set--quiet-p (name)
  "Return non-nil if NAME's last activity is older than the quiet threshold.
No recorded activity is not quiet — never-stamped sessions stay bright
until their first real edge, rather than the whole pool dimming on the
feature's first render."
  (when-let ((secs (bebop-set--age-seconds (bebop-set--last-activity name))))
    (> secs (* bebop-hud-quiet-hours 3600))))

(defface bebop-hud-unseen-face
  '((t :weight bold))
  "Emphasis overlay for a dynamic glyph that changed since last ack.
Composed in front of the glyph's base face; for nominal (shadow)
states it stands alone, lifting the glyph to default foreground."
  :group 'bebop)

(defun bebop-set--acked (name key)
  "Return NAME's persisted ack-snapshot value for KEY, or nil."
  (plist-get (bebop-set--session-meta name) key))

(defun bebop-set--unseen-status-p (name status)
  "Non-nil if NAME's live STATUS differs from its acked snapshot."
  (when-let ((acked (bebop-set--acked name :acked-status)))
    (not (equal acked (symbol-name status)))))

(defun bebop-set--unseen-mr-p (name key)
  "Non-nil if NAME's MR state KEY differs from its acked snapshot.
KEY is `bebop-mr--key' output or nil; \"none\" stands in for nil so
an MR appearing after an ack still reads as a change."
  (when-let ((acked (bebop-set--acked name :acked-mr)))
    (not (equal acked (or key "none")))))

(defun bebop-set-ack-session (name)
  "Snapshot NAME's dynamic glyph states as seen."
  (let ((live (cdr (assoc name bebop--live-sessions))))
    (bebop-set--update-session-meta
     name
     :acked-status (when-let ((s (and live (plist-get live :status))))
                     (symbol-name s))
     :acked-mr (or (bebop-mr--key (bebop-mr--entry name)) "none"))))

(defun bebop-set--ack-on-select (&rest _)
  "Ack the newly active session — visiting is the ack gesture."
  (when bebop--active-session
    (bebop-set-ack-session bebop--active-session)))

(advice-add 'bebop--apply-active-session :after #'bebop-set--ack-on-select)

(defun bebop-mark-all-seen ()
  "Ack every session's dynamic glyphs — the morning-after reset."
  (interactive)
  (dolist (row (bebop-set--rows))
    (bebop-set-ack-session (plist-get row :name)))
  (bebop--render)
  (message "Bebop: all sessions marked seen"))

(defun bebop-new-set (name)
  "Declare a new set NAME — the manual path for ticketless groupings."
  (interactive "sNew set name: ")
  (when (string-empty-p (string-trim name))
    (user-error "Set name cannot be empty"))
  (bebop-set--declare name)
  (bebop--render)
  (message "Set \"%s\" declared" name))

(defun bebop-remove-set (name)
  "Remove the empty set NAME from the registry.
Refuses if any session (running or on deck) still belongs to it —
reassign or exile members first. Removal is cheap: declarations are
self-healing, since enrichment re-creates a set the next time a
ticket matches its epic."
  (interactive
   (list (completing-read "Remove set: " (bebop-set--names) nil t)))
  (when (seq-some (lambda (r) (equal (plist-get r :set) name))
                  (bebop-set--rows))
    (user-error "Set %s still has members — reassign or exile them first"
                name))
  (setq bebop--sets (assoc-delete-all name bebop--sets))
  ;; Clear any stale meta references (sessions whose artifacts are gone
  ;; but whose choice entries linger).
  (dolist (cell bebop--session-meta)
    (when (equal (plist-get (cdr cell) :set) name)
      (setcdr cell (plist-put (cdr cell) :set nil))))
  (bebop-set--save)
  (bebop--render)
  (message "Set \"%s\" removed" name))

(defun bebop--entry-name-at-point ()
  "Return the session or on-deck entry name at point, or nil."
  (or (get-text-property (point) 'bebop-session-name)
      (get-text-property (point) 'bebop-on-deck-name)))

(defun bebop-assign-set (session set)
  "Assign SESSION to SET. Empty or nil SET clears the assignment.
Undeclared SET names are declared on the fly."
  (interactive
   (let* ((session (or (bebop--entry-name-at-point)
                       (completing-read "Session: "
                                        (mapcar #'car bebop--live-sessions)
                                        nil t)))
          (set (completing-read
                (format "Move %s to set (empty to ungroup): " session)
                (bebop-set--names))))
     (list session set)))
  (let ((set (and set (not (string-empty-p (string-trim set))) set)))
    (when set (bebop-set--declare set))
    (bebop-set--update-session-meta session :set set)
    (bebop--render)
    (message "%s → %s" session (or set "Ungrouped"))))

(defun bebop-set--row (name live-info)
  "Build a row plist for NAME. LIVE-INFO is the live-session plist or nil."
  (let* ((sinfo (bebop--session-info name))
         (chart (or (let ((c (and sinfo (plist-get sinfo :chart))))
                      (and c (file-exists-p c) c))
                    (let ((p (expand-file-name
                              (concat name ".org")
                              (expand-file-name bebop-charts-dir))))
                      (and (file-exists-p p) p))))
         (venue (or (let ((v (and sinfo (plist-get sinfo :venue))))
                      (and v (file-directory-p v) v))
                    (let ((p (expand-file-name
                              name (expand-file-name bebop-venues-dir))))
                      (and (file-directory-p p) p)))))
    (list :name name
          :live live-info
          :chart chart
          :venue venue
          :set (bebop-set--session-set name))))

(defun bebop-set--rows ()
  "Return rows for all running sessions and on-deck artifacts."
  (append
   (mapcar (lambda (pair) (bebop-set--row (car pair) (cdr pair)))
           bebop--live-sessions)
   (mapcar (lambda (name) (bebop-set--row name nil))
           (bebop--on-deck-names))))

(defun bebop-set--group (rows)
  "Group ROWS into (:live-ungrouped ROWS :sets ALIST :ungrouped ROWS).
Declared-but-empty sets render in :sets with no children."
  (let ((by-set (make-hash-table :test #'equal))
        ungrouped)
    (dolist (r rows)
      (let ((set (plist-get r :set)))
        (if set
            (puthash set (cons r (gethash set by-set)) by-set)
          (push r ungrouped))))
    (let (sets)
      (dolist (name (bebop-set--names))
        (push (cons name (nreverse (gethash name by-set))) sets))
      ;; Sets sort by ticket number descending (newest epics first);
      ;; ticketless sets follow, alphabetical.
      (let ((num (lambda (g)
                   (and (string-match "\\`[A-Z]+-\\([0-9]+\\)" (car g))
                        (string-to-number (match-string 1 (car g)))))))
        (setq sets (sort (nreverse sets)
                         (lambda (a b)
                           (let ((na (funcall num a)) (nb (funcall num b)))
                             (cond ((and na nb) (> na nb))
                                   (na t)
                                   (nb nil)
                                   (t (string< (car a) (car b)))))))))
      (let* ((sorted-un (bebop-set--sort-ungrouped (nreverse ungrouped)))
             ;; Running-but-ungrouped sessions are active work, not inbox
             ;; items — they render as a flat strip above the sets rather
             ;; than disappearing into the collapsed Ungrouped section.
             ;; The strip is plain-alphabetical.
             (live-un (sort (seq-filter
                             (lambda (r) (plist-get r :live))
                             sorted-un)
                            (lambda (a b)
                              (string< (plist-get a :name)
                                       (plist-get b :name))))))
        (list :live-ungrouped live-un
              :sets sets
              :ungrouped (seq-remove (lambda (r) (memq r live-un))
                                     sorted-un))))))

(defun bebop-set--sort-ungrouped (rows)
  "Sort ungrouped ROWS: live first, ticket-slug clusters together,
pinned utility names (emacs, bebop, claude) last."
  (let ((pinned '("emacs" "bebop" "claude")))
    (sort rows
          (lambda (a b)
            (let* ((na (plist-get a :name)) (nb (plist-get b :name))
                   (la (plist-get a :live)) (lb (plist-get b :live))
                   (pa (member na pinned)) (pb (member nb pinned))
                   (sa (or (bebop--on-deck-ticket-slug na) na))
                   (sb (or (bebop--on-deck-ticket-slug nb) nb)))
              (cond
               ((and la (not lb)) t)
               ((and lb (not la)) nil)
               ((and pa (not pb)) nil)
               ((and pb (not pa)) t)
               ((string= sa sb) (string< na nb))
               (t (string< sa sb))))))))

(defvar bebop-set--backline-ports-cache nil
  "Alist of (SLUG TIME . PORTS) caching backline port lookups.")

(defcustom bebop-set-backline-cache-ttl 10
  "Seconds to cache backline port lookups between dashboard renders."
  :type 'integer
  :group 'bebop)

(defun bebop-set--backline-ports (venue)
  "Return cached ports held by VENUE's backline, or nil."
  (when (and venue (fboundp 'bebop--backline-slugs))
    (let ((slug (file-name-nondirectory (directory-file-name venue))))
      (when (member slug (bebop--backline-slugs))
        (let ((hit (assoc slug bebop-set--backline-ports-cache)))
          (if (and hit (< (- (float-time) (cadr hit))
                          bebop-set-backline-cache-ttl))
              (cddr hit)
            (let ((ports (bebop--backline-ports slug)))
              (setq bebop-set--backline-ports-cache
                    (cons (cons slug (cons (float-time) ports))
                          (assoc-delete-all
                           slug bebop-set--backline-ports-cache)))
              ports)))))))

(defun bebop-set--dot-face (status)
  "Return the dot face for live STATUS — same mapping as the flat list."
  (cond
   ((memq status '(blocked waiting)) 'bebop-dot-waiting-face)
   ((memq status '(unknown degraded)) 'bebop-dot-degraded-face)
   (t 'bebop-dot-active-face)))

;; MR review status — the two-speed dashboard's slow lane. The left
;; gutter is local and live (every render); MR status is external
;; (GitLab), so it is fetched out of band by roundup, cached to a
;; sidecar, and only READ here. A cached status older than
;; `bebop-mr-stale-hours' renders dimmed so a stale snapshot never
;; masquerades as current.

(defcustom bebop-mr-cache-file (locate-user-emacs-file "bebop-mr-cache.json")
  "Sidecar file caching per-session MR review status.
Populated out of band by roundup; read-only from the render path."
  :type 'file
  :group 'bebop)

(defcustom bebop-mr-stale-hours 24
  "Hours after which a cached MR status renders dimmed rather than colored."
  :type 'number
  :group 'bebop)

(defvar bebop--mr-cache nil
  "Alist of (SESSION . PLIST). PLIST keys: :state :unresolved :iid :at.")

(defun bebop-mr--load ()
  "Load the MR status cache from `bebop-mr-cache-file'."
  (setq bebop--mr-cache nil)
  (when (file-exists-p bebop-mr-cache-file)
    (condition-case nil
        (let ((doc (with-temp-buffer
                     (insert-file-contents bebop-mr-cache-file)
                     (json-parse-buffer))))
          (maphash
           (lambda (name e)
             (push (cons name
                         (list :state (gethash "state" e)
                               :unresolved (or (gethash "unresolved" e) 0)
                               :iid (gethash "iid" e)
                               :at (gethash "at" e)))
                   bebop--mr-cache))
           doc))
      (error (setq bebop--mr-cache nil)))))

(defun bebop-mr--save ()
  "Persist the MR status cache."
  (let ((doc (make-hash-table :test #'equal)))
    (dolist (cell bebop--mr-cache)
      (let ((e (make-hash-table :test #'equal))
            (p (cdr cell)))
        (when (plist-get p :state) (puthash "state" (plist-get p :state) e))
        (puthash "unresolved" (or (plist-get p :unresolved) 0) e)
        (when (plist-get p :iid) (puthash "iid" (plist-get p :iid) e))
        (when (plist-get p :at) (puthash "at" (plist-get p :at) e))
        (puthash (car cell) e doc)))
    (with-temp-file bebop-mr-cache-file
      (insert (json-serialize doc)))))

(defun bebop-mr-cache-set (session state unresolved &optional iid)
  "Cache MR STATE (\"draft\"|\"open\"|\"merged\") and UNRESOLVED count for SESSION.
The citizen write verb roundup calls as a byproduct of its GitLab
fetch — the render path only reads. Stamps the current time."
  (let ((cell (assoc session bebop--mr-cache)))
    (unless cell
      (setq cell (cons session nil))
      (push cell bebop--mr-cache))
    (setcdr cell (list :state state
                       :unresolved (or unresolved 0)
                       :iid iid
                       :at (bebop--now-string))))
  (bebop-mr--save)
  (format "mr cached: %s" session))

(defun bebop-mr-clear (session)
  "Drop SESSION's cached MR status (e.g. after exile)."
  (interactive
   (list (completing-read "Clear MR status: "
                          (mapcar #'car bebop--mr-cache) nil t)))
  (setq bebop--mr-cache (assoc-delete-all session bebop--mr-cache))
  (bebop-mr--save))

(defun bebop-mr--entry (session)
  "Return SESSION's cached MR plist, or nil."
  (cdr (assoc session bebop--mr-cache)))

(defun bebop-mr--key (entry)
  "Return ENTRY's comparable state key, e.g. \"open:2\", or nil.
State plus unresolved count — a new comment on an unchanged MR is
still a change worth surfacing. This is what ack snapshots store."
  (when entry
    (format "%s:%d" (or (plist-get entry :state) "?")
            (or (plist-get entry :unresolved) 0))))

(defun bebop-mr--stale-p (entry)
  "Return non-nil if ENTRY's timestamp is older than `bebop-mr-stale-hours'.
Unparseable or missing timestamps count as stale — dim if unsure."
  (condition-case nil
      (let ((at (plist-get entry :at)))
        (or (null at)
            (> (- (float-time) (float-time (date-to-time at)))
               (* bebop-mr-stale-hours 3600))))
    (error t)))

(defun bebop-mr--gutter-glyph (session)
  "Return the single-char MR gutter glyph for SESSION, or nil if uncached.
◆ = unresolved teammate comments — colored even when acked, because
the comments still want answers; ✓ merged, ✎ draft, ◇ open render
dim — they ask nothing of you. A glyph whose state changed since your
last visit renders bold in full color until acked (the seen-unseen
mechanic; merged flashes green once, then dims). Stale entries render
dim regardless — never emphasize old news. The exact unresolved count
and MR number live in the glyph's tooltip (`help-echo`) — the gutter
stays one char wide."
  (when-let ((e (bebop-mr--entry session)))
    (let* ((state (plist-get e :state))
           (unresolved (or (plist-get e :unresolved) 0))
           (stale (bebop-mr--stale-p e))
           (unseen (and (not stale)
                        (bebop-set--unseen-mr-p session (bebop-mr--key e))))
           (spec (cond
                  ((> unresolved 0) (cons "◆" 'bebop-dot-waiting-face))
                  ((equal state "merged") (cons "✓" (and unseen 'bebop-dot-active-face)))
                  ((equal state "draft")  (cons "✎" nil))
                  ((equal state "open")   (cons "◇" nil))
                  (t nil)))
           (fc (cond
                (stale 'shadow)
                (unseen (if (cdr spec)
                            (list 'bebop-hud-unseen-face (cdr spec))
                          'bebop-hud-unseen-face))
                (t (or (cdr spec) 'shadow)))))
      (when spec
        (propertize (car spec)
                    'face fc 'font-lock-face fc
                    'help-echo (format "MR !%s · %s%s%s"
                                       (or (plist-get e :iid) "?")
                                       (or state "?")
                                       (if (> unresolved 0)
                                           (format " · %d unresolved" unresolved)
                                         "")
                                       (if stale " · stale" "")))))))

(defun bebop-set--prop (string face)
  "Propertize STRING with FACE as both `face' and `font-lock-face'.
font-lock is active in magit-section buffers and strips plain `face'
properties on refontification — setting both keeps colors, exactly as
magit's own `magit--propertize-face' does."
  (propertize string 'face face 'font-lock-face face))

(defun bebop-set--stop (column)
  "Return a stretch-space aligning subsequent text to COLUMN.
The gutter glyphs (⊞ 13px, ⎇ 16px) are wider than the 9px character
cell, so literal-space padding drifts; pixel column stops keep the
gutter grid exact on every line."
  (propertize " " 'display `(space :align-to ,column)))

(defconst bebop-set--name-col 8
  "Column where the name field begins, after the four-glyph gutter.
Each gutter glyph (dot 0, chart 2, venue 4, MR 6) gets a two-column
slot because the glyphs run wider than one character cell — a
one-column slot lets a wide glyph overrun its stop and shove the name
right, breaking alignment between rows that have the glyph and rows
that don't. All right-region anchors are measured from here.")

(defun bebop-set--gutter (row)
  "Return the fixed-column gutter string for ROW, or blank stops if nil.
Dot at column 0, chart glyph at 2, venue glyph at 4, MR glyph at 6,
text begins at 7. The MR column is the gutter's one cached, stale-able
member — everything left of it is live and derived; it dims when its
snapshot ages out (see `bebop-mr--gutter-glyph')."
  (if (null row)
      (bebop-set--stop bebop-set--name-col)
    (let* ((name (plist-get row :name))
           (live (plist-get row :live))
           (dot (if live
                    (let* ((status (plist-get live :status))
                           (base (bebop-set--dot-face status)))
                      (bebop-set--prop
                       "●" (if (bebop-set--unseen-status-p name status)
                               (list 'bebop-hud-unseen-face base)
                             base)))
                  (bebop-set--prop "○" 'shadow)))
           (chart (when (plist-get row :chart)
                    (bebop-set--prop "⊞" 'shadow)))
           (venue (when (plist-get row :venue)
                    (bebop-set--prop "⎇" 'shadow)))
           (mr (bebop-mr--gutter-glyph name)))
      (concat dot (bebop-set--stop 2)
              (or chart "") (bebop-set--stop 4)
              (or venue "") (bebop-set--stop 6)
              (or mr "") (bebop-set--stop bebop-set--name-col)))))

(defun bebop-set--name-width (rows set-names)
  "Return the shared name-column width anchoring the right region.
Must clear the longest thing on any line so the rollup/ports column
forms a clean vertical edge — including folded set headings. Session
rows sit under a 4-column indent, so they count as name+4; set-heading
names sit at the gutter edge, so they count as-is. Capped at 44; a
lone outlier past the cap simply overflows its own line."
  (min 44
       (max 24
            (+ 2 (max (apply #'max 0
                             (mapcar (lambda (r) (+ 4 (length (plist-get r :name))))
                                     rows))
                      (apply #'max 0 (mapcar #'length set-names)))))))

(defun bebop-set--insert-row (row width indent)
  "Insert one session line for ROW with name column WIDTH and INDENT."
  (let* ((name (plist-get row :name))
         (live (plist-get row :live))
         (face (cond
                ((and live (equal name bebop--active-session))
                 'bebop-selected-face)
                ((and live (bebop-set--quiet-p name)) 'shadow)
                (live 'bebop-session-face)
                (t 'shadow)))
         (ports (bebop-set--backline-ports (plist-get row :venue)))
         (age (bebop-set--age-string (bebop-set--last-activity name)))
         (start (point)))
    (magit-insert-section (bebop-session-row name)
      (insert (bebop-set--gutter row))
      (insert indent)
      (insert (bebop-set--prop name face))
      ;; Right region: backline ports anchored off the name column so
      ;; they form a true column down the tree; the age column hangs on
      ;; the window's right edge — the HUD's answer to "which of these
      ;; did I touch today." (MR status lives in the left gutter.)
      (when ports
        (insert (bebop-set--stop (+ bebop-set--name-col width)))
        (insert (bebop-set--prop
                 (mapconcat (lambda (p) (format ":%d" p)) ports " ")
                 'shadow)))
      (when age
        (insert (bebop-set--stop '(- right 5)))
        (insert (bebop-set--prop (format "%4s" age) 'shadow)))
      (insert "\n")
      (add-text-properties
       start (point)
       (if live
           (list 'bebop-session-name name)
         (list 'bebop-on-deck-name name))))))

(defun bebop-set--heading-counts (rows width)
  "Return iconographic, column-aligned rollup counts for a set heading.
Three fixed columns after the name field: the state icon repeated once
per session — ●● for two active (dot-active face), ● waiting
(dot-waiting face), ○○○ for three on deck; no numerals. Columns sit
at absolute stops so every set heading tables up regardless of name
length; a crowded cell overflows its stop with a single space keeping
it separated from the next. Counts are disjoint — a waiting session
is not also counted as running."
  (if (null rows)
      (concat (bebop-set--stop (+ bebop-set--name-col width))
              (bebop-set--prop "(empty)" 'shadow))
    (let* ((waiting (seq-count
                     (lambda (r)
                       (and (plist-get r :live)
                            (memq (plist-get (plist-get r :live) :status)
                                  '(waiting blocked))))
                     rows))
           (active (- (seq-count (lambda (r) (plist-get r :live)) rows)
                      waiting))
           (deck (seq-count (lambda (r) (not (plist-get r :live))) rows))
           (base (+ bebop-set--name-col width))
           (col 0)
           (parts nil))
      (dolist (cell (list (list active ?● 'bebop-dot-active-face)
                          (list waiting ?● 'bebop-dot-waiting-face)
                          (list deck ?○ 'shadow)))
        (push " " parts)
        (push (bebop-set--stop (+ base (* col 6))) parts)
        (when (> (car cell) 0)
          (push (bebop-set--prop (make-string (car cell) (cadr cell))
                                 (nth 2 cell))
                parts))
        (setq col (1+ col)))
      (apply #'concat (nreverse parts)))))

(defun bebop-set--counts (rows)
  "Return the textual rollup annotation for the Ungrouped heading."
  (if (null rows)
      "(empty)"
    (let* ((running (seq-count (lambda (r) (plist-get r :live)) rows))
           (deck (seq-count (lambda (r) (not (plist-get r :live))) rows))
           (pieces (delq nil
                         (list (when (> running 0) (format "%d running" running))
                               (when (> deck 0) (format "%d on deck" deck))))))
      (mapconcat #'identity pieces " · "))))

(defun bebop-set--insert-set (group width &optional hide)
  "Insert the section for GROUP (SET-NAME . ROWS). HIDE collapses it."
  (let* ((set (car group))
         (rows (cdr group))
         ;; The set's own session, by naming convention: a row named
         ;; exactly like the set renders as the heading's gutter.
         (own (seq-find (lambda (r) (equal (plist-get r :name) set)) rows))
         ;; copy-sequence before sort: sort is destructive and ROWS is
         ;; still iterated for the heading counts.
         (children (sort (copy-sequence (if own (remq own rows) rows))
                         (lambda (a b)
                           (string< (plist-get a :name)
                                    (plist-get b :name)))))
         (start (point)))
    (magit-insert-section (bebop-set set hide)
      (magit-insert-heading
        (concat (bebop-set--gutter own)
                (bebop-set--prop set '(:weight bold))
                (bebop-set--heading-counts rows width)))
      (when own
        (add-text-properties
         start (point)
         (if (plist-get own :live)
             (list 'bebop-session-name set)
           (list 'bebop-on-deck-name set))))
      (dolist (r children)
        (bebop-set--insert-row r width "    ")))))

(defun bebop-set--render-body ()
  "Insert the setlist tree — the dashboard body.
Order: live ungrouped sessions (flat strip), sets, Ungrouped."
  (let* ((rows (bebop-set--rows))
         (groups (bebop-set--group rows))
         (width (bebop-set--name-width rows (bebop-set--names))))
    (magit-insert-section (bebop-setlist)
      ;; Separator blank lines are inserted here in the ROOT section's
      ;; scope, between child sections — never inside a child, where
      ;; they would become foldable content and shift on toggle.
      (let ((strip (plist-get groups :live-ungrouped)))
        (dolist (r strip)
          (bebop-set--insert-row r width ""))
        (when (and strip (plist-get groups :sets))
          (insert "\n")))
      (dolist (g (plist-get groups :sets))
        (bebop-set--insert-set g width))
      (when-let ((un (plist-get groups :ungrouped)))
        (insert "\n")
        (magit-insert-section (bebop-ungrouped "Ungrouped" t)
          (magit-insert-heading
            (concat (bebop-set--stop bebop-set--name-col)
                    (bebop-set--prop "Ungrouped" '(:inherit shadow :weight bold))
                    (bebop-set--prop (format " (%s)" (bebop-set--counts un))
                                     'shadow)))
          (dolist (r un)
            (bebop-set--insert-row r width "    ")))))))

(setq bebop--render-body-function #'bebop-set--render-body)

(setq bebop-dashboard-footer-lines
      '("RET: select  TAB: fold"
        "n: new  k: kill  r: resume  e: exile"
        "C/D: chart  V/W: venue"
        "N: new set  m: move to set  g: refresh  .: mark seen"))

(define-key bebop-dashboard-mode-map (kbd "N") #'bebop-new-set)
(define-key bebop-dashboard-mode-map (kbd "m") #'bebop-assign-set)
(define-key bebop-dashboard-mode-map (kbd ".") #'bebop-mark-all-seen)

(bebop-set--load)
(bebop-mr--load)

(provide 'bebop-set)

;;; bebop-set.el ends here
