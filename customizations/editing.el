;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

;;;;
;; FUGALFUNKSTER
;;;;

;; automatically update buffers when a file changes on disk
(global-auto-revert-mode 1)

;; Org Mode
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   (emacs-lisp . t)
   (sh . t)
   ;;(elixir . t)
   (org . t)
   (ditaa . t)))

;; add support for exporting org docs as markdown
(require 'ox-md)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))
(setq org-src-fontify-natively t)
(deftheme org-beautify-theme "Sub-theme to beautify org mode")

;; fonts

(set-frame-font "CamingoCode 16")

;; yasnippets
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;JUMPing around

;; ace-window - move between windows easier
(global-set-key [remap other-window] 'ace-window)
(custom-set-faces
  '(aw-leading-char-face
      ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;; avy - place the cursor where you want to see it
;; TODO - how to use it
(require 'avy)
(global-set-key (kbd "C-s") 'avy-goto-char-timer)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-r") nil)
(global-set-key (kbd "C-r f") 'isearch-forward-regexp)
(global-set-key (kbd "C-r b") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
