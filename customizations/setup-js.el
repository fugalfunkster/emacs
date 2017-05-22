;; javascript / html

;;;;
;; FUGALFUNKSTER
;;;;

;; Recognize camelCaseWords as compositions of subwords
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(add-hook 'typescript-mode-hook 'subword-mode)


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
  ;; group backends to provide file completion, as well as completion from tide
  (setq-default company-backends '(company-files company-tide))
)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq-default tab-width 4)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :convertTabsToSpaces nil :tabSize 4))

;; init typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; should start tide when moving to web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;;
