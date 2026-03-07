;; Jira — org-jira

;; Syncs Jira tickets into org-mode files via =org-jira-get-issues-from-custom-jql=.
;; Tickets land in =org-jira-working-dir= (=~/Code/Org/=).


;; org-show-all was removed in Emacs 29; shim for org-jira compatibility
(unless (fboundp 'org-show-all)
  (defalias 'org-show-all 'org-fold-show-all))

(use-package org-jira
  :ensure t
  :config
  (setq jiralib-url "https://rentable.atlassian.net"
        jiralib-user-login-name "matthew@joinroost.com"
        org-jira-working-dir "~/Code/Org/"
        org-jira-use-status-as-todo t
        org-jira-custom-jqls
        '((:jql "project in (ROOST, SUP) AND assignee = currentUser() AND status not in (Closed, Done, Backlog) ORDER BY priority DESC"
                :limit 100 :filename "JIRA")))

  ;; Color-code Jira statuses so they're visually distinct
  (setq org-todo-keyword-faces
        '(("TODO"            . (:foreground "#ff6b6b" :weight bold))
          ("BLOCKED"         . (:foreground "#ff9944" :weight bold))
          ("BACKLOG"         . (:foreground "#888888" :weight normal))
          ("DESIGN/SPEC"     . (:foreground "#c792ea" :weight bold))
          ("DESK-REVIEW"     . (:foreground "#ffcb6b" :weight bold))
          ("CODE-REVIEW"     . (:foreground "#82aaff" :weight bold))
          ("IN-REVIEW"       . (:foreground "#89ddff" :weight bold))
          ("READY-TO-DEPLOY" . (:foreground "#c3e88d" :weight bold))
          ("DONE"            . (:foreground "#555555" :weight normal))
          ("CLOSED"          . (:foreground "#555555" :weight normal))))

  ;; org-jira switches to the project buffer at the end of every render.
  ;; Wrap it so the window configuration is restored when the callback completes,
  ;; and save the buffer so tickets persist across close/reopen.
  (advice-add 'org-jira--render-issues-from-issue-list :around
    (lambda (orig-fn issues)
      (save-window-excursion (funcall orig-fn issues))
      (with-current-buffer (org-jira--get-project-buffer (-last-item issues))
        (save-buffer))))

  ;; Override the per-project heading logic so all tickets land under a single
  ;; "* Tickets" heading regardless of project key.  This also eliminates the
  ;; async ordering bug — with one heading there's nowhere else to land.
  (advice-add 'org-jira--maybe-render-top-heading :override
    (lambda (_proj-key)
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Tickets" nil t)
        (goto-char (point-max))
        (insert "\n* Tickets\n"))
      (goto-char (point-min))
      (re-search-forward "^\\* Tickets" nil t)))

  ;; Prepend the Jira key (ROOST-5000) to the heading text so it appears
  ;; before the description rather than tucked away as a tag.
  (advice-add 'org-jira--render-issue :before
              (lambda (issue)
                (let* ((id (slot-value issue 'issue-id))
                       (hl (slot-value issue 'headline)))
                  (unless (string-prefix-p id hl)
                    (setf (slot-value issue 'headline)
                          (format "%s %s" id hl))))))

  (defun jira-sync ()
    "Clear the Tickets section in JIRA.org and fetch fresh from Jira.
Runs twice — org-jira is async: first call fires the HTTP request,
second (delayed) call renders the results."
    (interactive)
    (let ((jira-file (expand-file-name "JIRA.org" org-jira-working-dir)))
      (with-current-buffer (find-file-noselect jira-file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\* Tickets" nil t)
            (let ((start (line-beginning-position)))
              (org-end-of-subtree t t)
              (delete-region start (point)))))
        (save-buffer)))
    ;; jiralib--agile-call-async emits several progress messages on every page
    ;; of results.  Suppress all messages for the sync window, then restore.
    (setq inhibit-message t)
    (org-jira-get-issues-from-custom-jql)
    (run-with-timer 5 nil #'org-jira-get-issues-from-custom-jql)
    (run-with-timer 15 nil (lambda () (setq inhibit-message nil))))

  (defun jira-sort ()
    "Sort the * Tickets section by TODO status (DONE/CLOSED first, TODO last).
Re-collapses ticket subtrees after sorting so descriptions stay hidden."
    (interactive)
    (with-current-buffer (find-file-noselect
                          (expand-file-name "JIRA.org" org-jira-working-dir))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\* Tickets" nil t)
          (org-sort-entries nil ?O)
          ;; org-sort-entries expands subtrees; collapse back to headlines only
          (goto-char (point-min))
          (re-search-forward "^\\* Tickets" nil t)
          (outline-hide-subtree)
          (outline-show-children)))))

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c S") #'jira-sort)))

;; GitLab — forge

;; Magit extension with native GitLab (and GitHub) support. Surfaces MRs, issues,
;; and review comments as Magit sections; supports commenting, approving, and
;; merging from within Emacs.

;; Add an entry to =~/.authinfo.gpg= (the =^forge= suffix is required):

;; : machine gitlab.com login YOUR_USERNAME^forge password YOUR_PERSONAL_ACCESS_TOKEN

;; Token needs at minimum =api= scope.


;; TODO: enable forge
;; 1. Verify MELPA build is fixed (forge-topic-mark-read conflict as of early 2026)
;; 2. Add to ~/.authinfo:
;;    machine gitlab.com login YOUR_USERNAME^forge password YOUR_PERSONAL_ACCESS_TOKEN
;; 3. Uncomment below and tangle-and-restart
;;
(use-package forge
  :ensure t
  :after magit)
