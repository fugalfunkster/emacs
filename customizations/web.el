;;;;
;; FUGALFUNKSTER
;;;;

;; emmet-mode setup
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'typescript-mode-hook 'emmet-mode) ;; let's try emmet in ts-mode for TSX
(setq emmet-move-cursor-between-quotes t)
(setq emmet-expand-jsx-className? t)
(setq emmet-self-closing-tag-style " /")

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))

;; setup webmode for JSX/React
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)

(setq web-mode-enable-css-colorization t)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; web company mode
(add-hook 'web-mode-hook  'company-mode)
(add-hook 'scss-mode-hook  'company-mode)

;; chord for switch to web-mode
(global-set-key (kbd "C-c w") 'web-mode)


