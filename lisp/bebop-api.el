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
                    :ports (vconcat (plist-get st :ports)))))))
    (json-serialize
     (list :session name
           :set (or set :null)
           :chart (or (plist-get row :chart) :null)
           :venue (or venue :null)
           :ticket (or (bebop--on-deck-ticket-slug name) :null)
           :siblings (or siblings (vector))
           :backline (or backline :null)))))

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

(provide 'bebop-api)

;;; bebop-api.el ends here
