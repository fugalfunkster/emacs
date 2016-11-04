;;;;
;; FUGALFUNKSTER
;;;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; emmet-mode setup
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; setup webmode for JSX/React
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; chord for switch to web-mode
(global-set-key (kbd "C-c w") 'web-mode)

