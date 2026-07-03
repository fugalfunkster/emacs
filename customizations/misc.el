;;; misc.el --- Customizations -*- lexical-binding: t; -*-
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(setq create-lockfiles nil)

;; Avoid "ls does not support --dired" warning on macOS.
(setq dired-use-ls-dired nil)

;; Mark display-line-numbers as safe for boolean values in local variables.
;; Prevents the "apply local variables?" prompt for files like JIRA.org
;; that set display-line-numbers: nil in their Local Variables block.
(put 'display-line-numbers 'safe-local-variable #'booleanp)

(setq inhibit-startup-message t)

;; Exit fullscreen before quitting so macOS can tear down Spaces cleanly.
;; Without this, fullscreen frames leave black ghost spaces in Mission Control.
(add-hook 'kill-emacs-hook
          (lambda ()
            (dolist (frame (frame-list))
              (when (eq (frame-parameter frame 'fullscreen) 'fullscreen)
                (set-frame-parameter frame 'fullscreen nil)))
            (sleep-for 0.3)))

(run-with-idle-timer 5 nil
  (lambda ()
    (require 'keyfreq)
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))
