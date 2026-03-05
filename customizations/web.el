;; Web / Vue


(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'typescript-mode-hook 'emmet-mode)
(setq emmet-move-cursor-between-quotes t)
(setq emmet-expand-jsx-className? t)
(setq emmet-self-closing-tag-style " /")

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))

(setq web-mode-enable-css-colorization t)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(global-set-key (kbd "C-c w") 'web-mode)
(setq web-mode-markup-indent-offset 2)
