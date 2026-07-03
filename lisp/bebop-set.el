;;; bebop-set.el --- Sets, shelving, and the setlist dashboard tree -*- lexical-binding: t; -*-

(require 'bebop-core)
(require 'bebop-dashboard)
(require 'bebop-session)
(require 'magit-section)
(require 'seq)

;; Optional backline integration (port annotations); soft dependency.
(declare-function bebop--backline-slugs "bebop-backline")
(declare-function bebop--backline-ports "bebop-backline")

(defcustom bebop-set-state-file (locate-user-emacs-file "bebop-state.json")
  "File persisting set declarations, set membership, and shelved flags."
  :type 'file
  :group 'bebop)

(defvar bebop--sets nil
  "Alist of (SET-NAME . PLIST) for declared sets.
PLIST keys: :created-at.  Chart/repo/session attributes arrive lazily
in later phases; a set is at minimum a name.")

(defvar bebop--session-meta nil
  "Alist of (SESSION-NAME . PLIST) persisting per-session choices.
PLIST keys: :set (string or nil), :shelved (boolean).
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
        (when (plist-get (cdr cell) :shelved)
          (puthash "shelved" t entry))
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
                                      (when (eq (gethash "shelved" entry) t)
                                        (list :shelved t))))
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

(defun bebop-set--session-shelved-p (name)
  "Return non-nil if session NAME is shelved."
  (plist-get (bebop-set--session-meta name) :shelved))

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

(defun bebop-new-set (name)
  "Declare a new set NAME — the manual path for ticketless groupings."
  (interactive "sNew set name: ")
  (when (string-empty-p (string-trim name))
    (user-error "Set name cannot be empty"))
  (bebop-set--declare name)
  (bebop--render)
  (message "Set \"%s\" declared" name))

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

(defun bebop-shelve (name)
  "Shelve session NAME: hide it from the default dashboard view."
  (bebop-set--update-session-meta name :shelved t)
  (bebop--render))

(defun bebop-unshelve (name)
  "Unshelve session NAME."
  (bebop-set--update-session-meta name :shelved nil)
  (bebop--render))

(defun bebop-shelve-toggle-at-point ()
  "Toggle the shelved flag of the entry at point."
  (interactive)
  (let ((name (bebop--entry-name-at-point)))
    (unless name
      (user-error "No session at point"))
    (if (bebop-set--session-shelved-p name)
        (progn (bebop-unshelve name) (message "%s unshelved" name))
      (bebop-shelve name)
      (message "%s shelved" name))))

(defun bebop-set--set-at-point ()
  "Return the set name of the section at point, or of the entry at point."
  (or (when-let ((section (magit-current-section)))
        (let ((s section))
          (catch 'found
            (while s
              (when (eq (oref s type) 'bebop-set)
                (throw 'found (oref s value)))
              (setq s (oref s parent))))))
      (when-let ((name (bebop--entry-name-at-point)))
        (bebop-set--session-set name))))

(defun bebop-shelve-set-at-point ()
  "Shelve every session in the set at point (batch-flag).
If all of them are already shelved, unshelve them all instead."
  (interactive)
  (let ((set (bebop-set--set-at-point)))
    (unless set
      (user-error "No set at point"))
    (let* ((members (seq-filter (lambda (r) (equal (plist-get r :set) set))
                                (bebop-set--rows)))
           (names (mapcar (lambda (r) (plist-get r :name)) members))
           (all-shelved (and names
                             (seq-every-p #'bebop-set--session-shelved-p names))))
      (unless names
        (user-error "Set %s has no sessions" set))
      (dolist (n names)
        (bebop-set--update-session-meta n :shelved (not all-shelved)))
      (bebop--render)
      (message "Set %s: %s (%d sessions)"
               set (if all-shelved "unshelved" "shelved") (length names)))))

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
          :set (bebop-set--session-set name)
          :shelved (bebop-set--session-shelved-p name))))

(defun bebop-set--rows ()
  "Return rows for all running sessions and on-deck artifacts."
  (append
   (mapcar (lambda (pair) (bebop-set--row (car pair) (cdr pair)))
           bebop--live-sessions)
   (mapcar (lambda (name) (bebop-set--row name nil))
           (bebop--on-deck-names))))

(defun bebop-set--group (rows)
  "Group ROWS into (:sets ALIST :ungrouped ROWS :shelf ALIST).
A set lands on the shelf when it has members and every one is shelved.
Declared-but-empty sets render in :sets with no children."
  (let ((by-set (make-hash-table :test #'equal))
        ungrouped)
    (dolist (r rows)
      (let ((set (plist-get r :set)))
        (if set
            (puthash set (cons r (gethash set by-set)) by-set)
          (push r ungrouped))))
    (let (sets shelf)
      (dolist (name (bebop-set--names))
        (let ((members (nreverse (gethash name by-set))))
          (if (and members
                   (seq-every-p (lambda (r) (plist-get r :shelved)) members))
              (push (cons name members) shelf)
            (push (cons name members) sets))))
      ;; Sets with a live session first, then alphabetical.
      (let ((live-in (lambda (g)
                       (seq-some (lambda (r) (plist-get r :live)) (cdr g)))))
        (setq sets (sort (nreverse sets)
                         (lambda (a b)
                           (let ((la (funcall live-in a)) (lb (funcall live-in b)))
                             (cond ((and la (not lb)) t)
                                   ((and lb (not la)) nil)
                                   (t (string< (car a) (car b)))))))))
      (let* ((sorted-un (bebop-set--sort-ungrouped (nreverse ungrouped)))
             ;; Running-but-ungrouped sessions are active work, not inbox
             ;; items — they render as a flat strip above the sets rather
             ;; than disappearing into the collapsed Ungrouped section.
             (live-un (seq-filter (lambda (r) (and (plist-get r :live)
                                                   (not (plist-get r :shelved))))
                                  sorted-un)))
        (list :live-ungrouped live-un
              :sets sets
              :ungrouped (seq-remove (lambda (r) (memq r live-un)) sorted-un)
              :shelf (sort (nreverse shelf)
                           (lambda (a b) (string< (car a) (car b)))))))))

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

(defun bebop-set--gutter (row)
  "Return the fixed-column gutter string for ROW, or blank stops if nil.
Dot at column 0, chart glyph at 2, venue glyph at 4, text begins at 7."
  (if (null row)
      (bebop-set--stop 7)
    (let* ((live (plist-get row :live))
           (shelved (plist-get row :shelved))
           (dot (cond
                 (live (bebop-set--prop "●" (bebop-set--dot-face
                                             (plist-get live :status))))
                 (shelved (bebop-set--prop "○" '(:inherit shadow :slant italic)))
                 (t (bebop-set--prop "○" 'shadow))))
           (chart (when (plist-get row :chart)
                    (bebop-set--prop "⊞" 'shadow)))
           (venue (when (plist-get row :venue)
                    (bebop-set--prop "⎇" 'shadow))))
      (concat dot (bebop-set--stop 2)
              (or chart "") (bebop-set--stop 4)
              (or venue "") (bebop-set--stop 7)))))

(defun bebop-set--name-width (rows)
  "Return the name column width for ROWS.
Capped so a single long outlier name cannot push the annotation
column off screen; over-long names simply overflow their cell."
  (min 44
       (max 24 (+ 2 (apply #'max 0 (mapcar (lambda (r)
                                             (length (plist-get r :name)))
                                           rows))))))

(defun bebop-set--insert-row (row width indent)
  "Insert one session line for ROW with name column WIDTH and INDENT."
  (let* ((name (plist-get row :name))
         (live (plist-get row :live))
         (shelved (plist-get row :shelved))
         (face (cond
                (shelved '(:inherit shadow :slant italic))
                ((and live (equal name bebop--active-session))
                 'bebop-selected-face)
                (live 'bebop-session-face)
                (t 'shadow)))
         (ports (bebop-set--backline-ports (plist-get row :venue)))
         (start (point)))
    (magit-insert-section (bebop-session-row name)
      (insert (bebop-set--gutter row))
      (insert indent)
      (insert (bebop-set--prop name face))
      (when ports
        (insert (make-string (max 1 (- width (length name) (length indent))) ?\s))
        (insert (bebop-set--prop
                 (mapconcat (lambda (p) (format ":%d" p)) ports " ")
                 'shadow)))
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
(dot-waiting face), ○○○ for three on deck, ○ shelved (italic); no
numerals. Columns sit at absolute stops so every set heading tables
up regardless of name length; a crowded cell overflows its stop with
a single space keeping it separated from the next. Counts are
disjoint — a waiting session is not also counted as running."
  (if (null rows)
      (concat (bebop-set--stop (+ 7 width))
              (bebop-set--prop "(empty)" 'shadow))
    (let* ((waiting (seq-count
                     (lambda (r)
                       (and (plist-get r :live)
                            (memq (plist-get (plist-get r :live) :status)
                                  '(waiting blocked))))
                     rows))
           (active (- (seq-count (lambda (r) (plist-get r :live)) rows)
                      waiting))
           (shelved (seq-count (lambda (r) (plist-get r :shelved)) rows))
           (deck (seq-count (lambda (r) (and (not (plist-get r :live))
                                             (not (plist-get r :shelved))))
                            rows))
           (base (+ 7 width))
           (col 0)
           (parts nil))
      (dolist (cell (list (list active ?● 'bebop-dot-active-face)
                          (list waiting ?● 'bebop-dot-waiting-face)
                          (list deck ?○ 'shadow)
                          (list shelved ?○ '(:inherit shadow :slant italic))))
        (push " " parts)
        (push (bebop-set--stop (+ base (* col 6))) parts)
        (when (> (car cell) 0)
          (push (bebop-set--prop (make-string (car cell) (cadr cell))
                                 (nth 2 cell))
                parts))
        (setq col (1+ col)))
      (apply #'concat (nreverse parts)))))

(defun bebop-set--counts (rows)
  "Return the rollup annotation string for a set's ROWS."
  (if (null rows)
      "(empty)"
    (let* ((running (seq-count (lambda (r) (plist-get r :live)) rows))
           (waiting (seq-count (lambda (r)
                                 (and (plist-get r :live)
                                      (memq (plist-get (plist-get r :live) :status)
                                            '(waiting blocked))))
                               rows))
           (shelved (seq-count (lambda (r) (plist-get r :shelved)) rows))
           ;; Count directly — a row can be live AND shelved, so
           ;; total − running − shelved would double-subtract it.
           (deck (seq-count (lambda (r) (and (not (plist-get r :live))
                                             (not (plist-get r :shelved))))
                            rows))
           (pieces (delq nil
                         (list (when (> running 0) (format "%d running" running))
                               (when (> waiting 0) (format "%d waiting" waiting))
                               (when (> deck 0) (format "%d on deck" deck))
                               (when (> shelved 0) (format "%d shelved" shelved))))))
      (mapconcat #'identity pieces " · "))))

(defun bebop-set--insert-set (group width &optional hide)
  "Insert the section for GROUP (SET-NAME . ROWS). HIDE collapses it."
  (let* ((set (car group))
         (rows (cdr group))
         ;; The set's own session, by naming convention: a row named
         ;; exactly like the set renders as the heading's gutter.
         (own (seq-find (lambda (r) (equal (plist-get r :name) set)) rows))
         (children (if own (remq own rows) rows))
         (unshelved (seq-remove (lambda (r) (plist-get r :shelved)) children))
         (shelved (seq-filter (lambda (r) (plist-get r :shelved)) children))
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
      (dolist (r unshelved)
        (bebop-set--insert-row r width "    "))
      (when shelved
        (magit-insert-section (bebop-shelved (concat set "/shelved") t)
          (magit-insert-heading
            (concat (bebop-set--stop 7) "    "
                    (bebop-set--prop (format "… %d shelved" (length shelved))
                                     '(:inherit shadow :slant italic))))
          (dolist (r shelved)
            (bebop-set--insert-row r width "    ")))))))

(defun bebop-set--render-body ()
  "Insert the setlist tree — the dashboard body.
Order: live ungrouped sessions (flat strip), sets, Ungrouped, Shelf."
  (let* ((rows (bebop-set--rows))
         (groups (bebop-set--group rows))
         (width (bebop-set--name-width rows)))
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
            (concat (bebop-set--stop 7)
                    (bebop-set--prop "Ungrouped" '(:inherit shadow :weight bold))
                    (make-string (max 2 (- width 9)) ?\s)
                    (bebop-set--prop (bebop-set--counts un) 'shadow)))
          (dolist (r un)
            (bebop-set--insert-row r width "    "))))
      (when-let ((shelf (plist-get groups :shelf)))
        (insert "\n")
        (magit-insert-section (bebop-shelf "Shelf" t)
          (magit-insert-heading
            (concat (bebop-set--stop 7)
                    (bebop-set--prop "Shelf" '(:inherit shadow :weight bold))
                    (make-string (max 2 (- width 5)) ?\s)
                    (bebop-set--prop (format "%d sets" (length shelf)) 'shadow)))
          (dolist (g shelf)
            (bebop-set--insert-set g width t)))))))

(setq bebop--render-body-function #'bebop-set--render-body)

(setq bebop-dashboard-footer-lines
      '("n: new  N: new set  m: move to set  RET: select  TAB: fold"
        "k: kill  e: exile  a: archive  r: resume  C/D: chart  V/W: venue"
        "z: shelve  Z: shelve set  g: refresh  q: quit"))

(define-key bebop-dashboard-mode-map (kbd "N") #'bebop-new-set)
(define-key bebop-dashboard-mode-map (kbd "m") #'bebop-assign-set)
(define-key bebop-dashboard-mode-map (kbd "z") #'bebop-shelve-toggle-at-point)
(define-key bebop-dashboard-mode-map (kbd "Z") #'bebop-shelve-set-at-point)

(bebop-set--load)

(provide 'bebop-set)

;;; bebop-set.el ends here
