;; Jira — org-jira

;; Syncs Jira tickets into org-mode files via =org-jira-get-issues-from-custom-jql=.
;; Tickets land in =org-jira-working-dir= (=~/Code/Org/=).


(use-package org-jira
  :ensure t
  :config
  (setq jiralib2-url "https://rentable.atlassian.net"
        jiralib2-auth 'token
        jiralib2-user-login-name "matthew@joinroost.com"
        jiralib2-token (auth-source-pick-first-password
                        :host "rentable.atlassian.net"
                        :user "matthew@joinroost.com")
        org-jira-working-dir "~/Code/Org/"
        org-jira-default-jql "project = ROOST AND sprint in openSprints()"))

;; GitLab — forge

;; Magit extension with native GitLab (and GitHub) support. Surfaces MRs, issues,
;; and review comments as Magit sections; supports commenting, approving, and
;; merging from within Emacs.

;; Add an entry to =~/.authinfo.gpg= (the =^forge= suffix is required):

;; : machine gitlab.com login YOUR_USERNAME^forge password YOUR_PERSONAL_ACCESS_TOKEN

;; Token needs at minimum =api= scope.


(use-package forge
  :ensure t
  :after magit)
