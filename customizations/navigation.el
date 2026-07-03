;;; navigation.el --- Customizations -*- lexical-binding: t; -*-
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode 1)
(setq ido-default-buffer-method 'selected-window)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq smex-save-file (my-local-emacs-file ".smex-items"))
(use-package smex
  :defer t
  :commands (smex)
  :bind (("M-x" . smex))
  :config
  (smex-initialize))

(use-package projectile
  :defer t
  :hook (after-init . projectile-mode))
