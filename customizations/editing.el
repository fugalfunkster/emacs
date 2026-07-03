;;; editing.el --- Customizations -*- lexical-binding: t; -*-
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
(setq save-place-file (my-local-emacs-file "places"))

(setq backup-directory-alist `(("." . ,(my-local-emacs-file "backups"))))
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

(use-package company
  :defer t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-align-annotations t))

(with-eval-after-load 'company
  (defun my/eshell-company-setup ()
    "Make company less intrusive in Eshell."
    (set (make-local-variable 'company-active-map)
         (let ((map (copy-keymap company-active-map)))
           (define-key map (kbd "RET") nil)
           (define-key map (kbd "<return>") nil)
           (define-key map (kbd "C-m") nil)
           (define-key map (kbd "TAB") #'company-complete-selection)
           (define-key map (kbd "<tab>") #'company-complete-selection)
           (define-key map (kbd "<backtab>") #'company-select-previous)
           map)))
  (add-hook 'eshell-mode-hook #'my/eshell-company-setup))

(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Tab-line styling and behaviour. tab-line-mode is NOT enabled globally —
;; bebop-frame enables it selectively on the Conductor eshell buffers only.
;; These settings apply whenever tab-line is active anywhere.
(setq tab-line-tabs-function
      (lambda ()
        (seq-filter (lambda (b)
                      (eq (buffer-local-value 'major-mode b) 'eshell-mode))
                    (buffer-list))))

(setq tab-line-tab-name-function
      (lambda (buf &optional _tabs)
        ;; Prefer the label embedded in *eshell: LABEL* buffer names.
        ;; Fall back to abbreviated CWD for any other eshell buffer.
        (let ((name (buffer-name buf)))
          (if (string-match "\\*eshell: \\(.+\\)\\*" name)
              (format " %s " (match-string 1 name))
            (format " %s " (abbreviate-file-name
                              (directory-file-name
                               (buffer-local-value 'default-directory buf))))))))

(setq tab-line-close-button-show nil
      tab-line-new-button-show   nil)

;; Keep the current tab green even when the window is not selected.
;; Calls the default formatter first (preserving keymap/mouse-face/click handlers),
;; then overrides just the face so clicks still work.
(setq tab-line-tab-name-format-function
      (lambda (tab tabs)
        (let ((str (copy-sequence (tab-line-tab-name-format-default tab tabs))))
          (put-text-property 0 (length str) 'face
                             (if (get-buffer-window tab 'visible)
                                 'tab-line-tab-current
                               'tab-line-tab-inactive)
                             str)
          str)))

(with-eval-after-load 'tab-line
  (let* ((bg    (face-attribute 'default :background nil t))
         (fg    (face-attribute 'default :foreground nil t))
         (dim   (face-attribute 'shadow  :foreground nil t))
         ;; Use the same Goudy font as the bebop composition header.
         (goudy (seq-find (lambda (f) (member f (font-family-list)))
                          '("Goudy Mediaeval" "Goudy Medieval")))
         (font  (or goudy "CamingoCode")))
    (set-face-attribute 'tab-line nil
                        :family font :height 1.275
                        :background bg :foreground dim :box nil)
    (set-face-attribute 'tab-line-tab nil
                        :family font :height 1.275
                        :background bg :foreground dim :box nil)
    (set-face-attribute 'tab-line-tab-current nil
                        :family font :height 1.275
                        :background bg :foreground "#1aff66"
                        :weight 'bold :box nil)
    (set-face-attribute 'tab-line-tab-inactive nil
                        :family font :height 1.275
                        :background bg :foreground dim :box nil)
    (set-face-attribute 'tab-line-highlight nil
                        :background bg :foreground fg :box nil)))

;; C-u M-x eshell creates a new buffer via pop-to-buffer-same-window, which
;; loses to weakly-dedicated windows in the bebop conductor frame.
;; Force the new buffer into the selected window via set-window-buffer instead.
(advice-add 'eshell :around
  (lambda (orig-fn &optional arg)
    (let ((win (selected-window))
          (buf (save-window-excursion (funcall orig-fn arg))))
      (set-window-buffer win buf)
      buf)))

(global-set-key (kbd "s-{") #'tab-line-switch-to-prev-tab)
(global-set-key (kbd "s-}") #'tab-line-switch-to-next-tab)

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
(require 'ox-pandoc)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))
(setq org-src-fontify-natively t)
(deftheme org-beautify-theme "Sub-theme to beautify org mode")

(set-frame-font "CamingoCode 16")

(use-package yasnippet
  :defer t
  :load-path ("~/.emacs.d/plugins/yasnippet")
  :hook (after-init . yas-global-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets")))

(global-set-key [remap other-window] 'ace-window)
(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil
                      :font "CamingoCode"
                      :height 4.0
                      :weight 'bold))

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

(use-package olivetti
  :ensure t)
