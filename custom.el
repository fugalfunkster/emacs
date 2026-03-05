(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   '(company-files company-tide company-nxml company-css company-eclim
                   company-semantic company-clang company-xcode
                   company-cmake company-capf company-abbrev
                   company-oddmuse company-dabbrev))
 '(mode-require-final-newline nil)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   '(ace-window add-node-modules-path auto-compile auto-complete
                back-button base16-theme cider
                clojure-mode-extra-font-locking company dap-mode
                dart-server demo-it emmet-mode exec-path-from-shell
                expand-region flutter flycheck format-all
                gnu-elpa-keyring-update graphql-mode ido-ubiquitous
                js-comint js2-refactor lsp-dart magit multi-eshell
                neotree nodejs-repl ob-elixir ob-typescript
                org-beautify-theme org-bullets org-tree-slide paredit
                projectile rainbow-delimiters rainbow-mode rjsx-mode
                scss-mode smex tagedit tern tide undo-tree use-package
                vue-mode web-mode))
 '(require-final-newline nil)
 '(safe-local-variable-values
   '((eval let
           ((project-directory
             (car (dir-locals-find-file default-directory))))
           (setq lsp-clients-typescript-server-args
                 `("--tsserver-path"
                   ,(concat project-directory
                            ".yarn/sdks/typescript/bin/tsserver")
                   "--stdio"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
