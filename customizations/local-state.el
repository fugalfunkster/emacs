;;; local-state.el --- Customizations -*- lexical-binding: t; -*-
(defconst my-local-emacs-dir (expand-file-name "~/.emacs.d-local/"))
(unless (file-directory-p my-local-emacs-dir)
  (make-directory my-local-emacs-dir t))

(defun my-local-emacs-file (name)
  (expand-file-name name my-local-emacs-dir))

(dolist (dir (list (my-local-emacs-file "backups")
                   (my-local-emacs-file "auto-save")
                   (my-local-emacs-file "auto-save-list")
                   (my-local-emacs-file "url")
                   (my-local-emacs-file "transient")))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(setq recentf-save-file (my-local-emacs-file ".recentf"))
(setq ido-save-directory-list-file (my-local-emacs-file "ido.last"))
(setq smex-save-file (my-local-emacs-file ".smex-items"))
(setq save-place-file (my-local-emacs-file "places"))
(setq projectile-known-projects-file (my-local-emacs-file "projectile-bookmarks.eld"))
(setq package-user-dir (my-local-emacs-file "elpa/"))
(setq custom-file (my-local-emacs-file "custom.el"))
(setq forge-database-file (my-local-emacs-file "forge-database.sqlite"))
(setq keyfreq-file (my-local-emacs-file "keyfreq"))
(setq keyfreq-file-lock (my-local-emacs-file "keyfreq.lock"))

(setq backup-directory-alist `(("." . ,(my-local-emacs-file "backups"))))
(setq auto-save-file-name-transforms `((".*" ,(my-local-emacs-file "auto-save/") t)))
(setq auto-save-list-file-prefix (my-local-emacs-file "auto-save-list/.saves-"))

(setq url-configuration-directory (my-local-emacs-file "url/"))
(setq transient-history-file (my-local-emacs-file "transient/history.el"))
