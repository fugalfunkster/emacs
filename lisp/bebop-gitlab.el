;;; bebop-gitlab.el --- GitLab MR sync to Bebop charts -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'forge)
(require 'forge-pullreq)
(require 'bebop-session)
(require 'ghub)
(require 'url-util)

(defun bebop-gitlab--fetch-discussions (pr repo)
  "Return all MR discussions for PR via GitLab API.
Each element is a discussion alist with \\='id, \\='resolved, and \\='notes."
  (ghub-request "GET"
    (format "/projects/%s%%2F%s/merge_requests/%s/discussions"
            (url-hexify-string (oref repo owner))
            (url-hexify-string (oref repo name))
            (oref pr number))
    nil
    :forge 'gitlab
    :auth 'forge
    :host (oref repo apihost)
    :unpaginate t))

(defun bebop-gitlab--fetch-changes (pr repo)
  "Return hash table mapping file path to diff string for all changed files in PR."
  (let ((changes (alist-get 'changes
                   (ghub-request "GET"
                     (format "/projects/%s%%2F%s/merge_requests/%s/changes"
                             (url-hexify-string (oref repo owner))
                             (url-hexify-string (oref repo name))
                             (oref pr number))
                     nil
                     :forge 'gitlab
                     :auth 'forge
                     :host (oref repo apihost))))
        (table (make-hash-table :test #'equal)))
    (dolist (change changes)
      (let ((path (or (alist-get 'new_path change)
                      (alist-get 'old_path change)))
            (diff (alist-get 'diff change)))
        (when (and path diff)
          (puthash path diff table))))
    table))

(defun bebop-gitlab--markdown-to-org (text)
  "Normalize GitLab markdown TEXT to org format.
Converts code fences and common emoji shortcodes."
  (let ((text (string-trim (or text ""))))
    ;; Code fences: ```lang\ncode\n``` → #+begin_src lang\ncode\n#+end_src
    (while (string-match
            "```\\([a-zA-Z0-9_-]*\\)\n\\(\\(?:[^`]\\|`[^`]\\|``[^`]\\)*\\)\n?```"
            text)
      (let ((lang (match-string 1 text))
            (code (string-trim-right (match-string 2 text))))
        (setq text (concat (substring text 0 (match-beginning 0))
                           (if (string-empty-p lang)
                               (format "#+begin_src\n%s\n#+end_src" code)
                             (format "#+begin_src %s\n%s\n#+end_src" lang code))
                           (substring text (match-end 0))))))
    ;; Common emoji shortcodes
    (dolist (pair '((":smile:"  . "😊") (":laughing:"    . "😂")
                    (":thumbsup:" . "👍") (":\\+1:"       . "👍")
                    (":thumbsdown:" . "👎") (":-1:"       . "👎")
                    (":tada:"   . "🎉") (":rocket:"       . "🚀")
                    (":warning:" . "⚠️") (":white_check_mark:" . "✅")))
      (setq text (replace-regexp-in-string (car pair) (cdr pair) text t)))
    text))

(defun bebop-gitlab--diff-context (diff-str target-line context)
  "Return CONTEXT lines of code around TARGET-LINE from unified DIFF-STR.
Target line is prefixed with \"> \"; surrounding context lines with \"  \".
Returns nil when TARGET-LINE is not present in the diff."
  (when (and diff-str target-line (not (string-empty-p diff-str)))
    (let ((line-map (make-hash-table :test #'eql))
          (cur-line 0))
      (with-temp-buffer
        (insert diff-str)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((txt (buffer-substring-no-properties
                      (point) (line-end-position))))
            (cond
             ((string-match
               "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" txt)
              (setq cur-line (string-to-number (match-string 1 txt))))
             ;; Deletion lines: don't advance new-line counter
             ((and (> (length txt) 0) (= (aref txt 0) ?-)) nil)
             ;; Addition or context lines: record new-line → text
             ((and (> (length txt) 0)
                   (or (= (aref txt 0) ?+) (= (aref txt 0) ? )))
              (puthash cur-line (substring txt 1) line-map)
              (setq cur-line (1+ cur-line)))))
          (forward-line 1)))
      (when (gethash target-line line-map)
        (let ((lines '()))
          (cl-loop for n from (max 1 (- target-line context))
                   to (+ target-line context)
                   do (let ((txt (gethash n line-map)))
                        (when txt
                          (push (if (= n target-line)
                                    (format "> %s" txt)
                                  (format "  %s" txt))
                                lines))))
          (when lines
            (mapconcat #'identity (nreverse lines) "\n")))))))

(defun bebop-gitlab--mr-subtree-end (mr-pos)
  "Return buffer position at end of MR subtree rooted at MR-POS."
  (save-excursion
    (goto-char mr-pos)
    (org-end-of-subtree t t)))

(defun bebop-gitlab--find-or-create-mr-section
    (mr-number title url author api-host owner name)
  "Find or create a * MR Review: !MR-NUMBER heading in current buffer.
Returns position of the heading. Updates LAST_SYNCED when found."
  (let ((mr-pos nil))
    (org-map-entries
     (lambda ()
       (when (equal (number-to-string mr-number)
                    (org-entry-get nil "MR_NUMBER"))
         (setq mr-pos (point))))
     nil 'file)
    (if mr-pos
        (progn
          (goto-char mr-pos)
          (org-entry-put nil "LAST_SYNCED"
                         (format-time-string "%Y-%m-%dT%H:%M:%S"))
          mr-pos)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (setq mr-pos (point))
      (insert (format "* MR Review: !%s — %s\n" mr-number title))
      (insert "  :PROPERTIES:\n")
      (insert (format "  :GITLAB_URL: %s\n" url))
      (insert (format "  :MR_NUMBER: %s\n" mr-number))
      (insert (format "  :MR_AUTHOR: %s\n" author))
      (insert (format "  :API_HOST: %s\n" api-host))
      (insert (format "  :REPO_OWNER: %s\n" owner))
      (insert (format "  :REPO_NAME: %s\n" name))
      (insert (format "  :LAST_SYNCED: %s\n"
                      (format-time-string "%Y-%m-%dT%H:%M:%S")))
      (insert "  :END:\n")
      mr-pos)))

(defun bebop-gitlab--ensure-archived-section (mr-pos)
  "Return position of ** Archived heading under MR at MR-POS, creating if needed."
  (let ((found-pos nil))
    (save-excursion
      (goto-char mr-pos)
      (org-map-entries
       (lambda ()
         (when (and (= (org-current-level) 2)
                    (equal "Archived" (org-get-heading t t t t)))
           (setq found-pos (point))))
       nil 'tree))
    (unless found-pos
      (save-excursion
        (goto-char (bebop-gitlab--mr-subtree-end mr-pos))
        (unless (bolp) (insert "\n"))
        (insert "** Archived\n")
        (forward-line -1)
        (beginning-of-line)
        (setq found-pos (point))))
    found-pos))

(defun bebop-gitlab--find-or-create-file-section (mr-pos file-path)
  "Return position of ** FILE-PATH heading under MR section at MR-POS.
Creates it if absent. Pass \"General\" for notes without file position."
  (let ((file-pos nil))
    (save-excursion
      (save-restriction
        (narrow-to-region mr-pos (bebop-gitlab--mr-subtree-end mr-pos))
        (goto-char (point-min))
        (while (and (not file-pos)
                    (re-search-forward "^\\*\\* " nil t))
          (beginning-of-line)
          (when (equal file-path (org-get-heading t t t t))
            (setq file-pos (point)))
          (end-of-line))))
    (unless file-pos
      (let ((archived-pos nil)
            (mr-end (bebop-gitlab--mr-subtree-end mr-pos)))
        (save-excursion
          (save-restriction
            (narrow-to-region mr-pos mr-end)
            (goto-char (point-min))
            (when (re-search-forward "^\\*\\* Archived$" nil t)
              (setq archived-pos (line-beginning-position)))))
        (goto-char (or archived-pos mr-end))
        (unless (bolp) (insert "\n"))
        (setq file-pos (point))
        (insert (format "** %s\n" file-path))))
    file-pos))

(defun bebop-gitlab--upsert-comment
    (file-pos note discussion-id resolved-p note-index diff-map)
  "Insert or update a comment heading under the file section at FILE-POS.
NOTE is a note alist from the GitLab discussions API.
DISCUSSION-ID is the parent discussion\\='s id string.
RESOLVED-P is non-nil when the discussion is resolved.
NOTE-INDEX is 0 for the thread root, > 0 for replies.
DIFF-MAP maps file paths to diff strings for inline context.
Returns \\='new or \\='updated."
  (let* ((note-id   (number-to-string (alist-get 'id note)))
         (author    (or (alist-get 'username (alist-get 'author note)) "unknown"))
         (body      (bebop-gitlab--markdown-to-org
                     (or (alist-get 'body note) "")))
         (position  (alist-get 'position note))
         (file-path (and position (alist-get 'new_path position)))
         (new-line  (and position (alist-get 'new_line position)))
         (todo-kw   (if resolved-p "RESOLVED" "OPEN"))
         (found-pos nil)
         (found-lvl nil))
    (org-map-entries
     (lambda ()
       (when (equal note-id (org-entry-get nil "GITLAB_NOTE_ID"))
         (setq found-pos (point))
         (setq found-lvl (org-current-level))))
     nil 'file)
    (if found-pos
        ;; Update: flip OPEN↔RESOLVED on root notes only; never touch tags or body
        (progn
          (when (= (or found-lvl 0) 3)
            (save-excursion
              (goto-char found-pos)
              (beginning-of-line)
              (when (looking-at "^\\(\\*+\\) \\(OPEN\\|RESOLVED\\)\\( \\)")
                (replace-match (concat "\\1 " todo-kw "\\3")))))
          'updated)
      ;; Insert new note
      (let ((insert-pos
             (if (= note-index 0)
                 ;; Root: end of file section subtree
                 (save-excursion
                   (goto-char file-pos)
                   (org-end-of-subtree t t))
               ;; Reply: end of the root note's subtree for this discussion
               (let ((root-pos nil))
                 (org-map-entries
                  (lambda ()
                    (when (and (= (org-current-level) 3)
                               (equal discussion-id
                                      (org-entry-get nil "GITLAB_DISCUSSION_ID")))
                      (setq root-pos (point))))
                  nil 'file)
                 (save-excursion
                   (goto-char (or root-pos file-pos))
                   (org-end-of-subtree t t))))))
        (goto-char insert-pos)
        (unless (bolp) (insert "\n"))
        (if (= note-index 0)
            (let* ((loc      (if new-line (format "L%s" new-line) "general"))
                   (diff-ctx (when (and file-path new-line)
                               (bebop-gitlab--diff-context
                                (gethash file-path diff-map) new-line 2))))
              (insert (format "*** %s @%s — %s  :NEW:\n" todo-kw author loc))
              (insert "    :PROPERTIES:\n")
              (insert (format "    :GITLAB_NOTE_ID: %s\n" note-id))
              (insert (format "    :GITLAB_DISCUSSION_ID: %s\n" discussion-id))
              (insert "    :END:\n")
              (when diff-ctx
                (insert "    #+begin_quote\n")
                (dolist (ln (split-string diff-ctx "\n"))
                  (insert (format "    %s\n" ln)))
                (insert "    #+end_quote\n"))
              (dolist (ln (split-string body "\n"))
                (insert (format "    %s\n" ln))))
          (insert (format "**** @%s\n" author))
          (insert "     :PROPERTIES:\n")
          (insert (format "     :GITLAB_NOTE_ID: %s\n" note-id))
          (insert (format "     :GITLAB_DISCUSSION_ID: %s\n" discussion-id))
          (insert "     :END:\n")
          (dolist (ln (split-string body "\n"))
            (insert (format "     %s\n" ln))))
        'new))))

(defun bebop-gitlab--remove-deleted-comments (mr-pos valid-ids)
  "Delete headings whose GITLAB_NOTE_ID is absent from VALID-IDS hash table.
Operates within the MR subtree at MR-POS."
  (let ((to-delete '()))
    (save-excursion
      (goto-char mr-pos)
      (org-map-entries
       (lambda ()
         (let ((note-id (org-entry-get nil "GITLAB_NOTE_ID")))
           (when (and note-id (not (gethash note-id valid-ids)))
             (push (point-marker) to-delete))))
       nil 'tree))
    (dolist (marker (sort to-delete (lambda (a b)
                                      (> (marker-position a)
                                         (marker-position b)))))
      (goto-char (marker-position marker))
      (org-cut-subtree))))

(defun bebop-gitlab--archive-addressed-comments (mr-pos)
  "Move RESOLVED :ADDRESSED: level-3 headings to ** Archived under MR at MR-POS."
  (let ((to-archive '()))
    (save-excursion
      (goto-char mr-pos)
      (org-map-entries
       (lambda ()
         (when (and (= (org-current-level) 3)
                    (save-excursion
                      (beginning-of-line)
                      (looking-at "^\\*\\*\\* RESOLVED "))
                    (member "ADDRESSED" (org-get-tags nil t)))
           (push (point-marker) to-archive)))
       nil 'tree))
    (when to-archive
      (let ((texts '()))
        ;; Collect and delete in reverse order (highest position first)
        (dolist (marker (sort to-archive (lambda (a b)
                                           (> (marker-position a)
                                              (marker-position b)))))
          (goto-char (marker-position marker))
          (let ((end (save-excursion (org-end-of-subtree t t))))
            (push (buffer-substring-no-properties (point) end) texts)
            (delete-region (point) end)))
        ;; Append to ** Archived in original (top-to-bottom) order
        (let ((archived-pos (bebop-gitlab--ensure-archived-section mr-pos)))
          (save-excursion
            (goto-char archived-pos)
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (dolist (text (nreverse texts))
              (insert text))))))))

(defun bebop-gitlab--do-sync (discussions diff-map mr-pos)
  "Sync DISCUSSIONS into the chart at MR-POS using DIFF-MAP for diff context.
Returns (new-count . updated-count)."
  (let ((new-count 0)
        (updated-count 0)
        (valid-ids (let ((h (make-hash-table :test #'equal)))
                     (dolist (d discussions)
                       (dolist (n (alist-get 'notes d))
                         (let ((id (alist-get 'id n)))
                           (when id
                             (puthash (number-to-string id) t h)))))
                     h)))
    (bebop-gitlab--remove-deleted-comments mr-pos valid-ids)
    (dolist (discussion discussions)
      (let* ((disc-id     (alist-get 'id discussion))
             (resolved-p  (eq t (alist-get 'resolved discussion)))
             (valid-notes (cl-remove-if
                           (lambda (n)
                             (or (eq t (alist-get 'system n))
                                 (string-empty-p
                                  (string-trim
                                   (or (alist-get 'body n) "")))))
                           (alist-get 'notes discussion))))
        (cl-loop for note in valid-notes
                 for idx from 0
                 do (let* ((pos    (alist-get 'position note))
                           (fp     (or (and pos (alist-get 'new_path pos))
                                      "General"))
                           (fpos   (bebop-gitlab--find-or-create-file-section
                                    mr-pos fp))
                           (result (bebop-gitlab--upsert-comment
                                    fpos note disc-id resolved-p idx diff-map)))
                      (if (eq result 'new)
                          (cl-incf new-count)
                        (cl-incf updated-count))))))
    (bebop-gitlab--archive-addressed-comments mr-pos)
    (cons new-count updated-count)))

(defun bebop-gitlab--sync-mr-comments (pr repo url chart)
  "Sync all MR discussions from PR/REPO into CHART (file path).
URL is the full GitLab MR URL. Returns (new-count . updated-count)."
  (let* ((discussions (bebop-gitlab--fetch-discussions pr repo))
         (diff-map    (bebop-gitlab--fetch-changes pr repo))
         (result      nil))
    (with-current-buffer (find-file-noselect chart)
      (let ((mr-pos (bebop-gitlab--find-or-create-mr-section
                     (oref pr number)
                     (oref pr title)
                     url
                     (or (oref pr author) "unknown")
                     (oref repo apihost)
                     (oref repo owner)
                     (oref repo name))))
        (setq result (bebop-gitlab--do-sync discussions diff-map mr-pos))
        (save-buffer)))
    result))

(defun bebop-gitlab-sync-from-chart ()
  "Re-sync the MR Review section at point directly from GitLab.
Reads API_HOST, REPO_OWNER, REPO_NAME from the heading PROPERTIES.
Works from any heading inside the * MR Review section."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    ;; Walk up to the MR Review heading (identified by MR_NUMBER property)
    (while (and (not (org-entry-get nil "MR_NUMBER"))
                (> (org-current-level) 1))
      (outline-up-heading 1 t))
    (unless (org-entry-get nil "MR_NUMBER")
      (user-error "Not inside an MR Review section"))
    (let* ((mr-number (string-to-number (org-entry-get nil "MR_NUMBER")))
           (api-host  (or (org-entry-get nil "API_HOST")
                          (user-error "Missing API_HOST property")))
           (owner     (or (org-entry-get nil "REPO_OWNER")
                          (user-error "Missing REPO_OWNER property")))
           (repo-name (or (org-entry-get nil "REPO_NAME")
                          (user-error "Missing REPO_NAME property")))
           (chart     (or (buffer-file-name)
                          (user-error "Buffer not visiting a file — save it first")))
           (mr-pos    (point))
           (api-path  (format "/projects/%s%%2F%s/merge_requests/%s"
                              (url-hexify-string owner)
                              (url-hexify-string repo-name)
                              mr-number))
           (discussions
            (ghub-request "GET" (concat api-path "/discussions")
                          nil :forge 'gitlab :auth 'forge :host api-host
                          :unpaginate t))
           (diff-map
            (let ((h (make-hash-table :test #'equal)))
              (dolist (c (alist-get 'changes
                           (ghub-request "GET" (concat api-path "/changes")
                                         nil :forge 'gitlab :auth 'forge
                                         :host api-host)))
                (let ((p (or (alist-get 'new_path c) (alist-get 'old_path c)))
                      (d (alist-get 'diff c)))
                  (when (and p d) (puthash p d h))))
              h)))
      (org-entry-put nil "LAST_SYNCED" (format-time-string "%Y-%m-%dT%H:%M:%S"))
      (let ((counts (bebop-gitlab--do-sync discussions diff-map mr-pos)))
        (save-buffer)
        (message "MR !%s re-synced: %d new, %d updated"
                 mr-number (car counts) (cdr counts))))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c g") #'bebop-gitlab-sync-from-chart))

(defun bebop-gitlab-mr-to-chart (session-name)
  "Sync the forge MR at point to SESSION-NAME's chart.
First call creates the MR section; subsequent calls sync idempotently."
  (interactive
   (list (completing-read "Session: " (bebop--session-names) nil t)))
  (let* ((pr    (or (forge-pullreq-at-point) (user-error "No MR at point")))
         (repo  (forge-get-repository pr))
         (url   (format "https://%s/%s/%s/-/merge_requests/%s"
                        (oref repo githost)
                        (oref repo owner)
                        (oref repo name)
                        (oref pr number)))
         (chart (or (bebop--session-chart session-name)
                    (user-error "Session \"%s\" has no chart" session-name)))
         (result (bebop-gitlab--sync-mr-comments pr repo url chart)))
    (message "MR !%s synced: %d new, %d updated"
             (oref pr number) (car result) (cdr result))))

(provide 'bebop-gitlab)

;;; bebop-gitlab.el ends here
