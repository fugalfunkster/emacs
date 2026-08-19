;;; early-init.el --- runs before package activation -*- lexical-binding: t; -*-
(defconst my-local-emacs-dir (expand-file-name "~/.emacs.d-local/"))
(unless (file-directory-p my-local-emacs-dir)
  (make-directory my-local-emacs-dir t))
(setq package-user-dir (expand-file-name "elpa/" my-local-emacs-dir))

;; Third-party packages (org-jira, cider, …) emit benign "function not known to
;; be defined" warnings the first time the async native-compiler builds them —
;; the referenced functions are defined at runtime, just not visible at compile
;; time. Log them to *Native-Comp-Log* instead of popping *Warnings* on startup.
;; Must be set before the comp subsystem starts; a later defcustom won't override.
(setq native-comp-async-report-warnings-errors 'silent)

;; Keep native-comp .eln output off Dropbox too (same rationale as the elpa
;; redirect above): compiled artifacts otherwise churn through CloudStorage and
;; risk the same dataless-hashing failures. Must run before anything compiles.
;; Guard on the variable, not the function: startup-redirect-eln-cache exists
;; even in builds without native-comp, where calling it hits this void variable.
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" my-local-emacs-dir)))
