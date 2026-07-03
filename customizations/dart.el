;;; dart.el --- Customizations -*- lexical-binding: t; -*-
;; LSP settings are in lsp-common.el

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
