;;; lsp-common.el --- Shared LSP settings -*- lexical-binding: t; -*-

;; Performance tuning for LSP
(setq gc-cons-threshold (* 100 1024 1024)    ; 100 MB
      read-process-output-max (* 1024 1024)) ; 1 MB

;; Workaround: lsp-ada references this variable before defining it
(defvar lsp-ada-project-file nil)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-client-packages '(lsp-dart))
  (lsp-eldoc-enable-hover t)
  (lsp-signature-auto-activate nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-lens-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-completion-provider :capf)
  (lsp-enable-snippet t)
  (lsp-enable-indentation nil)              ; let major modes handle indentation
  (lsp-enable-on-type-formatting nil)       ; don't auto-format on type
  (lsp-auto-guess-root t)                   ; don't prompt for project root
  (lsp-session-file (expand-file-name ".lsp-session-v1" "~/.emacs.d-local/")) ; avoid Dropbox lock
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)         ; manual toggle via C-c i
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-peek-enable t))
