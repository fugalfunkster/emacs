;; javascript / web

;;;;
;; FUGALFUNKSTER
;;;;

;; Recognize camelCaseWords as compositions of subwords
(add-hook 'typescript-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'subword-mode)

;; prettier - long may it live
(require 'prettier-js)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'scss-mode-hook 'prettier-js-mode)
(add-hook 'css-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
     "--tab-width" "4"
     "--use-tabs" "true"
     "--single-quote" "true"))

;; Indent JS Modes
(setq js-indent-level 1)
(setq-default js2-basic-indent 1
	      js2-basic-offset 1
	      js2-auto-indent-p t
	      js2-cleanup-whitespace t
	      js2-enter-indents-newline t
	      js2-indent-on-enter-key t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "RET") 'js2-line-break))

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(global-set-key (kbd "C-c j") 'js2-mode)

;; flycheck hook
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jscs))
(setq-default flycheck-disabled-checkers '(sass/scss-sass-lint))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; typescript
(require 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (eldoc-mode +1)
  (company-mode +1)
  (setq tide-jump-to-definition-reuse-window nil)
)

(add-hook 'typescript-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x r") 'ts-send-region)
            (local-set-key (kbd "C-x C-r") 'ts-send-region-and-go)
            (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'ts-send-buffer)
            (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))

;; init typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; should start tide when moving to web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


