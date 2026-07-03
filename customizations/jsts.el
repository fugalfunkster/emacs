;;; jsts.el --- Customizations -*- lexical-binding: t; -*-
(require 'eldoc)
;; eldoc-documentation-functions was added in Emacs 28; shim it on 27.
(unless (boundp 'eldoc-documentation-functions)
  (defvar eldoc-documentation-functions nil
    "Compat shim for Emacs 27 — real variable added in Emacs 28."))

(add-hook 'typescript-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'subword-mode)

(add-hook 'js-mode-hook #'add-node-modules-path)
(add-hook 'js2-mode-hook #'add-node-modules-path)
(add-hook 'rjsx-mode-hook #'add-node-modules-path)
(add-hook 'typescript-mode-hook #'add-node-modules-path)
(add-hook 'web-mode-hook #'add-node-modules-path)

(setq js-indent-level 1)
(setq-default js2-basic-indent 1
	      js2-basic-offset 1
	      js2-auto-indent-p t
	      js2-cleanup-whitespace t
	      js2-enter-indents-newline t
	      js2-indent-on-enter-key t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "RET") 'js2-line-break))

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(global-set-key (kbd "C-c j") 'js2-mode)

(require 'flycheck)
(setq-default flycheck-disabled-checkers '(typescript-tslint javascript-jscs sass/scss-sass-lint))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

(require 'prettier-js)
(remove-hook 'js-mode-hook #'prettier-js)
(remove-hook 'js2-mode-hook #'prettier-js)
(remove-hook 'rjsx-mode-hook #'prettier-js)
(remove-hook 'typescript-mode-hook #'prettier-js)
(remove-hook 'before-save-hook #'jsts--prettier-format-buffer)
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (remove-hook 'before-save-hook #'jsts--prettier-format-buffer t)))

(defun jsts--prettier-js-before-save ()
  "Format the current buffer with prettier-js."
  (when (derived-mode-p 'js-mode 'js2-mode 'rjsx-mode 'typescript-mode)
    (prettier-js)))

(defun jsts--setup-prettier-js-on-save ()
  "Enable prettier-js formatting before save in this buffer."
  (remove-hook 'before-save-hook #'jsts--prettier-format-buffer t)
  (add-hook 'before-save-hook #'jsts--prettier-js-before-save nil t))

(defun jsts--maybe-setup-prettier-js-for-current-buffer ()
  "Enable prettier-js for JS/TS buffers."
  (when (derived-mode-p 'js-mode 'js2-mode 'rjsx-mode 'typescript-mode)
    (jsts--setup-prettier-js-on-save)))

(add-hook 'js-mode-hook #'jsts--setup-prettier-js-on-save)
(add-hook 'js2-mode-hook #'jsts--setup-prettier-js-on-save)
(add-hook 'rjsx-mode-hook #'jsts--setup-prettier-js-on-save)
(add-hook 'typescript-mode-hook #'jsts--setup-prettier-js-on-save)
(add-hook 'after-change-major-mode-hook #'jsts--maybe-setup-prettier-js-for-current-buffer)
(add-hook 'find-file-hook #'jsts--maybe-setup-prettier-js-for-current-buffer)

;;; --- Eglot for JS/TS ---

;; TSX → typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; JSX → web-mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(require 'eglot)

;; typescript-language-server for all JS/TS modes
(dolist (mode '(typescript-mode js-mode js2-mode rjsx-mode))
  (add-to-list 'eglot-server-programs
               `(,mode . ("typescript-language-server" "--stdio"))))

;; Auto-start eglot in JS/TS buffers
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'js-mode-hook #'eglot-ensure)
(add-hook 'js2-mode-hook #'eglot-ensure)
(add-hook 'rjsx-mode-hook #'eglot-ensure)

;; Don't let eglot manage formatting — we use prettier
(setq-default eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                   :documentRangeFormattingProvider
                                                   :documentOnTypeFormattingProvider))

;; Bridge eglot diagnostics into flycheck
(require 'flycheck-eglot)
(setq flycheck-eglot-exclusive nil)  ; keep eslint + other checkers alongside LSP
(global-flycheck-eglot-mode 1)

;;; --- Eglot keybindings ---

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l f") #'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c l o") #'eglot-code-action-organize-imports))

(global-set-key (kbd "C-c d") 'xref-find-definitions)
(global-set-key (kbd "C-c b") 'xref-pop-marker-stack)
