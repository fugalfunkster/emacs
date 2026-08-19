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
string, stamped by the HUD's timekeeping), :acked-status, :acked-mr,
and :acked-pipeline (the seen-unseen ack snapshot — glyph states as
of the last visit).
Keyed by name, independent of liveness — applies equally to running
sessions and on-deck artifacts.")

(defvar bebop--exile-proposals nil
  "List of session names queued for exile approval.
Populated by roundup (via `bebop-propose-exile' in bebop-api);
consumed interactively. Persisted with the other choices.")

(defvar bebop-set--inhibit-save nil
  "Non-nil while batching meta updates; `bebop-set--save' becomes a no-op.
Bind it around a burst of `bebop-set--update-session-meta' calls and
save once after — N acks should not mean N file writes.")

(defun bebop-set--save ()
  "Persist sets and session choices to `bebop-set-state-file'."
  (unless bebop-set--inhibit-save
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
        (when-let ((a (plist-get (cdr cell) :acked-pipeline)))
          (puthash "acked-pipeline" a entry))
        (when (> (hash-table-count entry) 0)
          (puthash (car cell) entry sessions))))
    (puthash "version" 2 doc)
    (puthash "sets" sets doc)
    (puthash "sessions" sessions doc)
    (when bebop--exile-proposals
      (puthash "proposals" (vconcat bebop--exile-proposals) doc))
    (with-temp-file bebop-set-state-file
      (insert (json-serialize doc))))))

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
                                        (list :acked-mr a))
                                      (when-let ((a (gethash "acked-pipeline" entry)))
                                        (list :acked-pipeline a))))
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

(defun bebop-set--unseen-pipeline-p (name key)
  "Non-nil if NAME's pipeline KEY differs from its acked snapshot.
Same \"none\" convention as `bebop-set--unseen-mr-p'."
  (when-let ((acked (bebop-set--acked name :acked-pipeline)))
    (not (equal acked (or key "none")))))

(defun bebop-set-ack-session (name)
  "Snapshot NAME's dynamic glyph states as seen."
  (let ((live (cdr (assoc name bebop--live-sessions))))
    (bebop-set--update-session-meta
     name
     :acked-status (when-let ((s (and live (plist-get live :status))))
                     (symbol-name s))
     :acked-mr (or (bebop-mr--key (bebop-mr--entry name)) "none")
     :acked-pipeline (or (bebop-pipeline--key (bebop-pipeline--entry name))
                         "none"))))

(defun bebop-set--ack-on-select (&rest _)
  "Ack the newly active session — visiting is the ack gesture."
  (when bebop--active-session
    (bebop-set-ack-session bebop--active-session)))

(advice-add 'bebop--apply-active-session :after #'bebop-set--ack-on-select)

(defun bebop-mark-all-seen ()
  "Ack every session's dynamic glyphs — the morning-after reset.
Saves once, not once per session."
  (interactive)
  (let ((bebop-set--inhibit-save t))
    (dolist (row (bebop-set--rows))
      (bebop-set-ack-session (plist-get row :name))))
  (bebop-set--save)
  (bebop--render)
  (message "Bebop: all sessions marked seen"))

(defconst bebop-set--attention-order '(blocked waiting pipeline comments)
  "Attention kinds, worst first — the marquee's severity scale.")

(defun bebop-set--attention-kinds (row)
  "Return ROW's unacked attention kinds, worst first, or nil.
A list drawn from `bebop-set--attention-order'.  The single
membership truth behind the marquee (non-nil = listed, car = rank),
the set-heading ◉ tally, and the periphery counts — one predicate,
three consumers, no drift.  Comments only count on an open or draft
MR: a closed or merged MR's threads ask nothing of you."
  (let* ((name (plist-get row :name))
         (live (plist-get row :live))
         (status (and live (plist-get live :status)))
         kinds)
    (let ((e (bebop-mr--entry name)))
      (when (and e
                 (member (plist-get e :state) '("open" "draft"))
                 (> (or (plist-get e :unresolved) 0) 0)
                 (not (bebop-mr--stale-p e))
                 (bebop-set--unseen-mr-p name (bebop-mr--key e)))
        (push 'comments kinds)))
    (let ((p (bebop-pipeline--entry name)))
      (when (and p (equal (plist-get p :status) "failed")
                 (not (bebop-pipeline--stale-p p))
                 (bebop-set--unseen-pipeline-p
                  name (bebop-pipeline--key p)))
        (push 'pipeline kinds)))
    (when (and (memq status '(waiting blocked))
               (bebop-set--unseen-status-p name status))
      (push (if (eq status 'blocked) 'blocked 'waiting) kinds))
    kinds))

(defun bebop-set--marquee-p (row)
  "Non-nil when ROW holds an unacked attention state."
  (bebop-set--attention-kinds row))

(defun bebop-set--marquee-rank (row)
  "Return ROW's severity rank — the position of its worst kind."
  (let ((kinds (bebop-set--attention-kinds row)))
    (if kinds
        (seq-position bebop-set--attention-order (car kinds))
      (length bebop-set--attention-order))))

(defun bebop-set--marquee< (a b)
  "Order marquee rows A and B worst-first, then most recent first.
ISO timestamps compare lexicographically; never-stamped rows sink."
  (let ((ra (bebop-set--marquee-rank a))
        (rb (bebop-set--marquee-rank b)))
    (if (/= ra rb) (< ra rb)
      (string> (or (bebop-set--last-activity (plist-get a :name)) "")
               (or (bebop-set--last-activity (plist-get b :name)) "")))))

(defun bebop-hud--notify (fmt &rest args)
  "Ding and echo FMT formatted with ARGS, prefixed \"Bebop: \".
The `bebop--notify-waiting' cousin for external edges."
  (ding t)
  (message "Bebop: %s" (apply #'format fmt args)))

(defvar bebop-hud--periphery nil
  "Cached mode-line segment, or nil while the marquee is dark.
Recomputed by `bebop-hud--update-periphery' at render time.")

(defun bebop-hud--update-periphery (rows)
  "Recompute the periphery counts from ROWS and refresh mode-lines.
Counts come from `bebop-set--attention-kinds' — the same membership
truth as the marquee, so the mode-line can never disagree with the
lane."
  (let ((dots 0) (pipes 0) (mrs 0))
    (dolist (r rows)
      (let ((kinds (bebop-set--attention-kinds r)))
        (when (or (memq 'blocked kinds) (memq 'waiting kinds))
          (setq dots (1+ dots)))
        (when (memq 'pipeline kinds)
          (setq pipes (1+ pipes)))
        (when (memq 'comments kinds)
          (setq mrs (1+ mrs)))))
    (setq bebop-hud--periphery
          (when (> (+ dots pipes mrs) 0)
            (concat " "
                    (mapconcat
                     #'identity
                     (delq nil
                           (list
                            (when (> dots 0)
                              (bebop-set--prop (format "● %d" dots)
                                               'bebop-dot-waiting-face))
                            (when (> pipes 0)
                              (bebop-set--prop (format "✗ %d" pipes)
                                               'bebop-dot-waiting-face))
                            (when (> mrs 0)
                              (bebop-set--prop (format "◆ %d" mrs)
                                               'bebop-dot-waiting-face))))
                     "  ")
                    " "))))
  (force-mode-line-update t))

(defun bebop-hud--modeline ()
  "Return the periphery segment for `global-mode-string'."
  (or bebop-hud--periphery ""))

(add-to-list 'global-mode-string '(:eval (bebop-hud--modeline)) t)

(defcustom bebop-hud-pulse-swaps 3
  "Face swaps in a transition pulse. Zero disables pulsing entirely."
  :type 'integer
  :group 'bebop)

(defcustom bebop-hud-pulse-interval 0.12
  "Seconds between transition-pulse frames.
Total pulse length is roughly twice this times `bebop-hud-pulse-swaps'
— half a second, the length of a glance."
  :type 'number
  :group 'bebop)

(defface bebop-hud-pulse-face
  '((((background dark)) :background "#3A3A3A")
    (((background light)) :background "#DCDCDC"))
  "Grayscale wash marking a row that just changed state.
Background only: the row must not reflow mid-flicker, and a transition
is a change of emphasis, not a new state worth a color."
  :group 'bebop)

(defvar bebop-hud--pulses nil
  "Alist of session NAME to a plist (:timer :overlay :frames).
Holds only in-flight pulses; entries delete themselves on their last
frame, so a quiet dashboard carries no pulse state at all.")

(defun bebop-hud--pulse-clear (name)
  "End NAME's pulse: cancel its timer, delete its overlay, drop its entry."
  (when-let ((state (cdr (assoc name bebop-hud--pulses))))
    (when (timerp (plist-get state :timer))
      (cancel-timer (plist-get state :timer)))
    (when (overlayp (plist-get state :overlay))
      (delete-overlay (plist-get state :overlay)))
    (setq bebop-hud--pulses (assoc-delete-all name bebop-hud--pulses))))

(defun bebop-hud--pulse-detach ()
  "Delete every pulse overlay, keeping the timers running.
Called from the render path: rebuilding the buffer strands the
overlays where they sat, and the next frame re-locates its row
anyway."
  (dolist (entry bebop-hud--pulses)
    (let ((ov (plist-get (cdr entry) :overlay)))
      (when (overlayp ov) (delete-overlay ov))
      (plist-put (cdr entry) :overlay nil))))

(defun bebop-hud--pulse-row (name)
  "Return (BEG . END) of NAME's topmost dashboard row, or nil.
Topmost is deliberate: when the marquee is projecting the session, the
pulse lands in the lane — the place the eye is already being sent."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (if (equal name (get-text-property (point) 'bebop-session-name))
            (setq found (cons (line-beginning-position)
                              (line-end-position)))
          (forward-line 1)))
      found)))

(defun bebop-hud--pulse-frame (name)
  "Render one pulse frame for NAME and count down toward the last.
Odd frames wash, even frames rest, so the row alternates; the frame
after the last one clears the pulse. A dead dashboard buffer ends the
pulse rather than resurrecting it."
  (when-let ((state (cdr (assoc name bebop-hud--pulses))))
    (let ((buf (get-buffer bebop-buffer-name))
          (frames (plist-get state :frames))
          (ov (plist-get state :overlay)))
      (when (overlayp ov) (delete-overlay ov))
      (plist-put state :overlay nil)
      (if (or (<= frames 0) (not (buffer-live-p buf)))
          (bebop-hud--pulse-clear name)
        (plist-put state :frames (1- frames))
        (when (= 1 (mod frames 2))
          (with-current-buffer buf
            (when-let ((row (bebop-hud--pulse-row name)))
              (let ((new (make-overlay (car row) (cdr row) buf)))
                (overlay-put new 'face 'bebop-hud-pulse-face)
                (overlay-put new 'priority 100)
                (plist-put state :overlay new)))))))))

(defun bebop-hud--pulse (name)
  "Pulse NAME's dashboard row once — a short grayscale flicker.
Restarts rather than stacks: a burst of transitions on one session can
never accumulate timers. No dashboard buffer, no pulse."
  (bebop-hud--pulse-clear name)
  (when (and (> bebop-hud-pulse-swaps 0)
             (buffer-live-p (get-buffer bebop-buffer-name)))
    (let ((state (list :timer nil :overlay nil
                       :frames (1- (* 2 bebop-hud-pulse-swaps))))
          timer)
      (push (cons name state) bebop-hud--pulses)
      ;; The repeating timer holds itself so it can commit suicide if
      ;; its entry disappears from under it — a module reload mid-pulse
      ;; would otherwise leave a repeating timer with nothing to do.
      (setq timer (run-at-time
                   bebop-hud-pulse-interval bebop-hud-pulse-interval
                   (lambda ()
                     (if (assoc name bebop-hud--pulses)
                         (bebop-hud--pulse-frame name)
                       (cancel-timer timer)))))
      (plist-put state :timer timer))))

(defun bebop-hud--pulse-on-activity (name event)
  "Pulse NAME on a state transition. Hook target.
Sends are excluded: you do not need the dashboard to blink at your own
keystroke — only at an edge that happened while you looked away."
  (when (eq event 'transition)
    (bebop-hud--pulse name)))

(add-hook 'bebop-session-activity-functions #'bebop-hud--pulse-on-activity)

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

;; External status — the two-speed dashboard's slow lane. The left
;; gutter is local and live (every render); MR review and pipeline
;; status are external (GitLab), so they are fetched out of band —
;; pulse on its heartbeat, roundup during its audit — written through
;; the citizen verbs, and only READ here. A cached status older than
;; its stale threshold renders dimmed so an old snapshot never
;; masquerades as current. The sidecar lives under the Dropbox-synced
;; org tree, named for this host: a Live Remote client can read the
;; host's snapshot at native speed without touching GitLab.

(defcustom bebop-external-cache-file
  (expand-file-name
   (format "../.bebop/%s.status.json"
           (car (split-string (system-name) "\\.")))
   (expand-file-name bebop-charts-dir))
  "Sidecar caching per-session external status (MR review, pipeline).
Written by out-of-band fetchers through the citizen verbs; read-only
from the render path. Derived from `bebop-charts-dir' rather than a
hardcoded path so it lands in the synced org tree on every machine —
hosts mount Dropbox at different points, and an unsynced sidecar
would silently defeat the Live Remote read-at-the-cafe pattern."
  :type 'file
  :group 'bebop)

(defcustom bebop-mr-stale-hours 24
  "Hours after which a cached MR status renders dimmed rather than colored."
  :type 'number
  :group 'bebop)

(defcustom bebop-pipeline-stale-hours 2
  "Hours after which a cached pipeline status renders dimmed.
Short by design: pipelines move fast, and a dim glyph doubles as the
signal that pulse has stopped beating."
  :type 'number
  :group 'bebop)

(defvar bebop--external-cache nil
  "Alist of (SESSION . PLIST). PLIST keys: :mr :pipeline.
:mr is (:state :unresolved :iid :at); :pipeline is (:status :at).")

(defun bebop-external--load ()
  "Load the external-status cache, importing the old MR sidecar once.
The pre-pulse cache lived at bebop-mr-cache.json under
`user-emacs-directory' and held bare MR entries; if the new file
doesn't exist yet, fold the old one in and write it."
  (setq bebop--external-cache nil)
  (cond
   ((file-exists-p bebop-external-cache-file)
    (condition-case nil
        (let* ((doc (with-temp-buffer
                      (insert-file-contents bebop-external-cache-file)
                      (json-parse-buffer)))
               (sessions (gethash "sessions" doc)))
          (when sessions
            (maphash
             (lambda (name e)
               (push (cons name
                           (append
                            (when-let ((m (gethash "mr" e)))
                              (list :mr
                                    (list :state (gethash "state" m)
                                          :unresolved (or (gethash "unresolved" m) 0)
                                          :iid (gethash "iid" m)
                                          :at (gethash "at" m))))
                            (when-let ((p (gethash "pipeline" e)))
                              (list :pipeline
                                    (list :status (gethash "status" p)
                                          :at (gethash "at" p))))))
                     bebop--external-cache))
             sessions)))
      (error (setq bebop--external-cache nil))))
   ((file-exists-p (locate-user-emacs-file "bebop-mr-cache.json"))
    (condition-case nil
        (let ((doc (with-temp-buffer
                     (insert-file-contents
                      (locate-user-emacs-file "bebop-mr-cache.json"))
                     (json-parse-buffer))))
          (maphash
           (lambda (name e)
             (push (cons name
                         (list :mr
                               (list :state (gethash "state" e)
                                     :unresolved (or (gethash "unresolved" e) 0)
                                     :iid (gethash "iid" e)
                                     :at (gethash "at" e))))
                   bebop--external-cache))
           doc)
          (bebop-external--save))
      (error (setq bebop--external-cache nil))))))

(defun bebop-external--save ()
  "Persist the external-status cache."
  (let ((sessions (make-hash-table :test #'equal))
        (doc (make-hash-table :test #'equal)))
    (dolist (cell bebop--external-cache)
      (let ((e (make-hash-table :test #'equal)))
        (when-let ((m (plist-get (cdr cell) :mr)))
          (let ((h (make-hash-table :test #'equal)))
            (when (plist-get m :state) (puthash "state" (plist-get m :state) h))
            (puthash "unresolved" (or (plist-get m :unresolved) 0) h)
            (when (plist-get m :iid) (puthash "iid" (plist-get m :iid) h))
            (when (plist-get m :at) (puthash "at" (plist-get m :at) h))
            (puthash "mr" h e)))
        (when-let ((p (plist-get (cdr cell) :pipeline)))
          (let ((h (make-hash-table :test #'equal)))
            (when (plist-get p :status) (puthash "status" (plist-get p :status) h))
            (when (plist-get p :at) (puthash "at" (plist-get p :at) h))
            (puthash "pipeline" h e)))
        (when (> (hash-table-count e) 0)
          (puthash (car cell) e sessions))))
    (puthash "version" 1 doc)
    (puthash "sessions" sessions doc)
    (make-directory (file-name-directory bebop-external-cache-file) t)
    (with-temp-file bebop-external-cache-file
      (insert (json-serialize doc)))))

(defun bebop-external--cell (session)
  "Return SESSION's cache cell, creating it if absent."
  (or (assoc session bebop--external-cache)
      (let ((cell (cons session nil)))
        (push cell bebop--external-cache)
        cell)))

(defun bebop-mr-cache-set (session state unresolved &optional iid)
  "Cache MR STATE (\"draft\"|\"open\"|\"merged\") and UNRESOLVED count for SESSION.
Citizen write verb — pulse and roundup call it as a byproduct of
their GitLab fetches; the render path only reads. Stamps now.
A rising unresolved count dings (see Periphery); a first observation
never does — a transition needs a before."
  (let* ((cell (bebop-external--cell session))
         (prior (plist-get (cdr cell) :mr))
         (old (and prior (or (plist-get prior :unresolved) 0)))
         (new (or unresolved 0)))
    (setcdr cell (plist-put (cdr cell) :mr
                            (list :state state
                                  :unresolved new
                                  :iid iid
                                  :at (bebop--now-string))))
    (when (and old (> new old))
      (bebop-hud--notify "new MR comment on %s (%d unresolved)"
                         session new)))
  (bebop-external--save)
  (format "mr cached: %s" session))

(defun bebop-pipeline-cache-set (session status)
  "Cache head-pipeline STATUS (\"success\"|\"failed\"|\"running\"|...) for SESSION.
Citizen write verb, the pipeline twin of `bebop-mr-cache-set'.
Stamps now. A flip to failed dings (see Periphery); a first
observation never does."
  (let* ((cell (bebop-external--cell session))
         (old (plist-get (plist-get (cdr cell) :pipeline) :status)))
    (setcdr cell (plist-put (cdr cell) :pipeline
                            (list :status status
                                  :at (bebop--now-string))))
    (when (and old (not (equal old "failed")) (equal status "failed"))
      (bebop-hud--notify "pipeline failed — %s" session)))
  (bebop-external--save)
  (format "pipeline cached: %s" session))

(defun bebop-external-clear (session)
  "Drop SESSION's cached external status (e.g. after exile)."
  (interactive
   (list (completing-read "Clear external status: "
                          (mapcar #'car bebop--external-cache) nil t)))
  (setq bebop--external-cache
        (assoc-delete-all session bebop--external-cache))
  (bebop-external--save))

(defalias 'bebop-mr-clear #'bebop-external-clear
  "Kept for callers that predate the pipeline generalization.")

(defun bebop-mr--entry (session)
  "Return SESSION's cached MR plist, or nil."
  (plist-get (cdr (assoc session bebop--external-cache)) :mr))

(defun bebop-pipeline--entry (session)
  "Return SESSION's cached pipeline plist, or nil."
  (plist-get (cdr (assoc session bebop--external-cache)) :pipeline))

(defun bebop-mr--key (entry)
  "Return ENTRY's comparable state key, e.g. \"open:2\", or nil.
State plus unresolved count — a new comment on an unchanged MR is
still a change worth surfacing. This is what ack snapshots store."
  (when entry
    (format "%s:%d" (or (plist-get entry :state) "?")
            (or (plist-get entry :unresolved) 0))))

(defun bebop-external--stale-p (entry hours)
  "Return non-nil if ENTRY's :at is older than HOURS.
Unparseable or missing timestamps count as stale — dim if unsure."
  (condition-case nil
      (let ((at (plist-get entry :at)))
        (or (null at)
            (> (- (float-time) (float-time (date-to-time at)))
               (* hours 3600))))
    (error t)))

(defun bebop-mr--stale-p (entry)
  "Return non-nil if ENTRY is older than `bebop-mr-stale-hours'."
  (bebop-external--stale-p entry bebop-mr-stale-hours))

(defun bebop-pipeline--stale-p (entry)
  "Return non-nil if ENTRY is older than `bebop-pipeline-stale-hours'."
  (bebop-external--stale-p entry bebop-pipeline-stale-hours))

(defun bebop-pipeline--key (entry)
  "Return ENTRY's comparable state key — the raw status string, or nil.
Status alone: a re-run that lands on the same status is not a change."
  (plist-get entry :status))

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
                  ;; ◆ only for open/draft: comments on a closed or
                  ;; merged MR ask nothing of you.
                  ((and (> unresolved 0)
                        (member state '("open" "draft")))
                   (cons "◆" 'bebop-dot-waiting-face))
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

(defun bebop-pipeline--gutter-glyph (session)
  "Return the single-char pipeline gutter glyph for SESSION, or nil.
✗ failed — red even when acked, because the build is still broken;
⟳ running-ish states; ✓ passed renders dim — a green build asks
nothing of you (it flashes green once while unseen, like a merged
MR). Other statuses (canceled, skipped, manual) cache but render
nothing — deliberate: they ask nothing and earn no ink. Unseen
changes render bold in full color until acked; stale entries render
dim regardless, and staleness here doubles as pulse's own health
meter. The raw status lives in the tooltip."
  (when-let ((e (bebop-pipeline--entry session)))
    (let* ((status (plist-get e :status))
           (stale (bebop-pipeline--stale-p e))
           (unseen (and (not stale)
                        (bebop-set--unseen-pipeline-p
                         session (bebop-pipeline--key e))))
           (spec (cond
                  ((equal status "failed")
                   (cons "✗" 'bebop-dot-waiting-face))
                  ((equal status "success")
                   (cons "✓" (and unseen 'bebop-dot-active-face)))
                  ((member status '("running" "pending" "created"
                                    "preparing" "waiting_for_resource"))
                   (cons "⟳" nil))
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
                    'help-echo (format "pipeline · %s%s"
                                       (or status "?")
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

(defconst bebop-set--name-col 10
  "Column where the name field begins, after the five-glyph gutter.
Each gutter glyph (dot 0, chart 2, venue 4, MR 6, pipeline 8) gets a
two-column slot because the glyphs run wider than one character cell
— a one-column slot lets a wide glyph overrun its stop and shove the
name right, breaking alignment between rows that have the glyph and
rows that don't. All right-region anchors are measured from here.")

(defun bebop-set--gutter (row)
  "Return the fixed-column gutter string for ROW, or blank stops if nil.
Dot at column 0, chart glyph at 2, venue glyph at 4, MR glyph at 6,
pipeline glyph at 8, text begins at 9. The MR and pipeline columns
are the gutter's cached, stale-able members — everything left of them
is live and derived; they dim when their snapshots age out (see
`bebop-mr--gutter-glyph' and `bebop-pipeline--gutter-glyph')."
  (if (null row)
      (bebop-set--stop bebop-set--name-col)
    (let* ((name (plist-get row :name))
           (live (plist-get row :live))
           (dot (if live
                    (let* ((status (plist-get live :status))
                           ;; Dot decay: past `bebop-hud-dot-stale-days'
                           ;; the status color is spent, not the status.
                           (base (if (bebop--dot-stale-p name)
                                     'shadow
                                   (bebop-set--dot-face status))))
                      (bebop-set--prop
                       "●" (if (bebop-set--unseen-status-p name status)
                               (list 'bebop-hud-unseen-face base)
                             base)))
                  (bebop-set--prop "○" 'shadow)))
           (chart (when (plist-get row :chart)
                    (bebop-set--prop "⊞" 'shadow)))
           (venue (when (plist-get row :venue)
                    (bebop-set--prop "⎇" 'shadow)))
           (mr (bebop-mr--gutter-glyph name))
           (pipe (bebop-pipeline--gutter-glyph name)))
      (concat dot (bebop-set--stop 2)
              (or chart "") (bebop-set--stop 4)
              (or venue "") (bebop-set--stop 6)
              (or mr "") (bebop-set--stop 8)
              (or pipe "") (bebop-set--stop bebop-set--name-col)))))

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
         ;; Age earns its ink only once the row has gone quiet: a
         ;; five-minute-old session is the normal case, not a signal.
         ;; The two staleness cues then agree — bright name and no age,
         ;; or shadow name and a visible age, never a mix.
         (age (and (bebop-set--quiet-p name)
                   (bebop-set--age-string (bebop-set--last-activity name))))
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
Four fixed columns after the name field: the state icon repeated once
per session — ●● for two active (dot-active face), ● waiting
(dot-waiting face), ○○○ for three on deck, ◉ for one unacked
attention state (bold, so even the fold line admits what it's sitting
on); no numerals. Columns sit at absolute stops so every set heading
tables up regardless of name length; a crowded cell overflows its
stop with a single space keeping it separated from the next. The
first three counts are disjoint — a waiting session is not also
counted as running; the unacked column overlaps them by design."
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
           (unacked (seq-count #'bebop-set--marquee-p rows))
           (base (+ bebop-set--name-col width))
           (col 0)
           (parts nil))
      (dolist (cell (list (list active ?● 'bebop-dot-active-face)
                          (list waiting ?● 'bebop-dot-waiting-face)
                          (list deck ?○ 'shadow)
                          (list unacked ?◉
                                (list 'bebop-hud-unseen-face
                                      'bebop-dot-waiting-face))))
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
Order: marquee (only when lit), live ungrouped sessions (flat strip),
sets, Ungrouped. Marquee rows are projections — the same session
renders again in its home section; both carry the text properties
RET reads, so activation works from either listing."
  (let* ((rows (bebop-set--rows))
         (groups (bebop-set--group rows))
         (width (bebop-set--name-width rows (bebop-set--names))))
    ;; The buffer was just erased under any in-flight pulse; drop the
    ;; stranded overlays and let the running timers re-locate their rows.
    (bebop-hud--pulse-detach)
    (magit-insert-section (bebop-setlist)
      ;; Separator blank lines are inserted here in the ROOT section's
      ;; scope, between child sections — never inside a child, where
      ;; they would become foldable content and shift on toggle. The
      ;; marquee label is likewise a plain line, not a section heading —
      ;; folding away attention would defeat the lane.
      (bebop-hud--update-periphery rows)
      (let ((marquee (sort (seq-filter #'bebop-set--marquee-p rows)
                           #'bebop-set--marquee<)))
        (when marquee
          (insert (concat (bebop-set--stop bebop-set--name-col)
                          (bebop-set--prop "Marquee"
                                           '(:inherit shadow :weight bold))
                          "\n"))
          (dolist (r marquee)
            (bebop-set--insert-row r width ""))
          (insert "\n")))
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
(bebop-external--load)

(provide 'bebop-set)

;;; bebop-set.el ends here
