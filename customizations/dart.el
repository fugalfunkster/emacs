;; Dart / Flutter


(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package use-package-ensure-system-package :ensure t)
(use-package projectile :ensure t)
(use-package yasnippet :ensure t :config (yas-global-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
  '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
    lsp-ui company hover))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil)

(use-package dart-mode
  :ensure t
  :hook ((dart-mode . flutter-test-mode)
         (dart-mode . format-all-mode)
         (dart-mode . lsp))
  :ensure-system-package (dart . dart-sdk))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map ("C-M-x" . #'flutter-run-or-hot-reload))
  :hook (dart-mode . (lambda ()
                       (add-hook 'after-save-hook #'flutter-hot-reload nil t))))
