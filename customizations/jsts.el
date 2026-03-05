;; JavaScript / TypeScript


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

(require 'tide)
(defun jsts--tide-should-enable-p ()
  "Return non-nil when a JS/TS project config exists."
  (when buffer-file-name
    (let ((root (or (locate-dominating-file buffer-file-name "tsconfig.json")
                    (locate-dominating-file buffer-file-name "jsconfig.json"))))
      (and root t))))

(defun jsts--maybe-setup-tide ()
  "Enable tide when this buffer is part of a JS/TS project."
  (when (jsts--tide-should-enable-p)
    (setup-tide-mode)))

(defun jsts--maybe-setup-tide-for-current-buffer ()
  "Enable tide for JS/TS buffers when appropriate."
  (when (derived-mode-p 'js-mode 'js2-mode 'rjsx-mode 'typescript-mode)
    (jsts--maybe-setup-tide)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (jsts--disable-eldoc)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (company-mode +1)
  (setq tide-jump-to-definition-reuse-window nil))

(add-hook 'typescript-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x r") 'ts-send-region)
            (local-set-key (kbd "C-x C-r") 'ts-send-region-and-go)
            (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'ts-send-buffer)
            (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)

(add-hook 'js2-mode-hook #'jsts--maybe-setup-tide)
(add-hook 'rjsx-mode-hook #'jsts--maybe-setup-tide)
(add-hook 'js-mode-hook #'jsts--maybe-setup-tide)
(add-hook 'after-change-major-mode-hook #'jsts--maybe-setup-tide-for-current-buffer)
(add-hook 'find-file-hook #'jsts--maybe-setup-tide-for-current-buffer)

(defvar-local jsts--saved-eldoc-documentation-functions nil
  "Buffer-local storage for eldoc documentation functions.")

(defun jsts--capture-eldoc-functions ()
  "Capture eldoc documentation functions for later use."
  (setq-local jsts--saved-eldoc-documentation-functions
              (or eldoc-documentation-functions
                  (and (fboundp 'tide-eldoc-function)
                       (list #'tide-eldoc-function)))))

(defun jsts--disable-eldoc ()
  "Disable eldoc in the current buffer."
  (unless jsts--saved-eldoc-documentation-functions
    (setq jsts--saved-eldoc-documentation-functions
          (or eldoc-documentation-functions
              (and (fboundp 'tide-eldoc-function)
                   (list #'tide-eldoc-function))
              (default-value 'eldoc-documentation-functions))))
  (setq-local eldoc-documentation-functions nil)
  (eldoc-mode -1))

(defun jsts--eldoc-show-once ()
  "Show eldoc buffer for symbol at point without enabling auto popups."
  (setq jsts--saved-eldoc-documentation-functions
        (or eldoc-documentation-functions
            (and (fboundp 'tide-eldoc-function)
                 (list #'tide-eldoc-function))
            jsts--saved-eldoc-documentation-functions
            (default-value 'eldoc-documentation-functions)))
  (when jsts--saved-eldoc-documentation-functions
    (setq-local eldoc-documentation-functions
                jsts--saved-eldoc-documentation-functions)
    (eldoc-mode 1)
    (when (fboundp 'eldoc-print-current-symbol-info)
      (eldoc-print-current-symbol-info))
    (eldoc-doc-buffer)
    (jsts--disable-eldoc)
    (setq jsts--saved-eldoc-documentation-functions
          (or (and (fboundp 'tide-eldoc-function)
                   (list #'tide-eldoc-function))
              jsts--saved-eldoc-documentation-functions))))

(add-hook 'tide-mode-hook #'jsts--capture-eldoc-functions)
(add-hook 'tide-mode-hook #'jsts--disable-eldoc)
(add-hook 'js-mode-hook #'jsts--disable-eldoc)
(add-hook 'js2-mode-hook #'jsts--disable-eldoc)
(add-hook 'rjsx-mode-hook #'jsts--disable-eldoc)
(add-hook 'typescript-mode-hook #'jsts--disable-eldoc)

(add-hook 'web-mode-hook
          (lambda ()
            (when (and (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
                       (jsts--tide-should-enable-p))
              (setup-tide-mode))))

(global-set-key (kbd "C-c d") 'xref-find-definitions)
(global-set-key (kbd "C-c b") 'xref-pop-marker-stack)
