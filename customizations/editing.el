;; Editing


(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(show-paren-mode 1)
(global-hl-line-mode 1)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default nil)

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-tooltip-align-annotations t)

(global-auto-revert-mode 1)

(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   (emacs-lisp . t)
   (elixir . t)
   (C . t)
   (org . t)
   (ditaa . t)
   (shell . t)))

(require 'ox-md)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))
(setq org-src-fontify-natively t)
(deftheme org-beautify-theme "Sub-theme to beautify org mode")

(set-frame-font "CamingoCode 16")

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(global-set-key [remap other-window] 'ace-window)

(require 'avy)
(global-set-key (kbd "C-s") 'avy-goto-char-timer)

(global-set-key (kbd "C-r") nil)
(global-set-key (kbd "C-r f") 'isearch-forward-regexp)
(global-set-key (kbd "C-r b") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-a") 'back-to-indentation)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "<M-DEL>") 'backward-delete-word)

(defun delete-whole-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))
(global-set-key (kbd "M-k") 'delete-whole-line)

(global-set-key (kbd "C-k") 'kill-whole-line)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-." . mc/mark-next-like-this)
         ("C-," . mc/unmark-next-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))
