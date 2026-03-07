;; UI


(menu-bar-mode -1)
(tool-bar-mode -1)

(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

(set-face-attribute 'default nil :height 130)

(setq initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 48)))

(setq x-select-enable-clipboard t
      x-select-enable-primary nil
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(blink-cursor-mode 0)
(setq-default frame-title-format "%b (%f)")
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; I know what scratch is for
(setq initial-scratch-message ";; Perfection is achieved not when there is nothing more to add,\n;; but when there is nothing left to take away. - Antoine de Saint-Exupery")

(setq ring-bell-function 'ignore)

;; Org visual tweaks — reduce the "wall of orange headers" effect.
;; org-hide-leading-stars keeps only the last * visible per heading.
;; org-indent-mode virtually indents subtrees so nesting is obvious without
;; relying on heading level alone to convey structure.
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook #'org-indent-mode)
