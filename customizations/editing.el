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

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

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

;; I don't always use tabs, but when I do they are 4 spaces long
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; When you need to change from tabs to spaces
;; (defun die-tabs ()
;;   (interactive)
;;   (set-variable 'tab-width 2)
;;   (mark-whole-buffer)
;;   (untabify (region-beginning) (region-end))
;;   (keyboard-quit))

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

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; automatically update buffers when a file changes on disk
(global-auto-revert-mode 1)

;; Org Mode
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   (emacs-lisp . t)
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
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

;;JUMPing around

;; ace-window - move between windows easier
(global-set-key [remap other-window] 'ace-window)
(custom-set-faces
  '(aw-leading-char-face
      ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;; avy - place the cursor where you want to see it
(require 'avy)
(global-set-key (kbd "C-s") 'avy-goto-char-timer)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-r") nil)
(global-set-key (kbd "C-r f") 'isearch-forward-regexp)
(global-set-key (kbd "C-r b") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; change default behavior for C-a to M-m
(global-set-key (kbd "C-a") 'back-to-indentation)

;; replace backward-kill-word with backward-delete-word
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
;; note paredit overwrites this macro to backward-kill-word :(

;; now that I can't easily "C-a C-SPACE C-e DEL" lets...
;; delete or kill a whole line
(defun delete-whole-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))
(global-set-key (kbd "M-k") 'delete-whole-line)

;; and, kill a whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-." . mc/mark-next-like-this)
         ("C-," . mc/unmark-next-like-this)
         ("C-<mouse-1>" . mc/add-cursor-on-click)))

;; expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))
