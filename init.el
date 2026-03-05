;;;;
;; Packages
;;;;

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(use-package
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    ido-ubiquitous
    smex
    projectile
    rainbow-delimiters
    tagedit
    magit
    org
    org-beautify-theme
    org-bullets
    ob-typescript
    ob-elixir
    js2-mode
    js2-refactor
    js-comint
    nodejs-repl
    tern
    web-mode
    typescript-mode
    tide
    lsp-mode
    lsp-dart
    lsp-ui
    hover
    flycheck
    yasnippet
    auto-complete
    company
    direnv
    multi-eshell
    neotree
    avy
    ace-window
    emmet-mode
    expand-region
    multiple-cursors
    undo-tree
    restart-emacs))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/lisp")

(defun mh/xref-find-definitions-buffer ()
  "Show xref results for symbol at point in a separate buffer."
  (interactive)
  (let ((xref-show-xrefs-function #'xref-show-definitions-buffer)
        (xref-show-definitions-function #'xref-show-definitions-buffer))
    (call-interactively #'xref-find-definitions)))

(defun mh/tide-doc-toggle-buffer ()
  "Toggle a Tide documentation buffer for symbol at point."
  (interactive)
  (let* ((buf (get-buffer "*tide-doc*"))
         (win (and buf (get-buffer-window buf))))
    (if win
        (quit-window nil win)
      (when (and (bound-and-true-p tide-mode)
                 (fboundp 'tide-eldoc-function))
        (let ((doc (tide-eldoc-function)))
          (when (and doc (stringp doc) (not (string-empty-p doc)))
            (with-current-buffer (get-buffer-create "*tide-doc*")
              (setq buffer-read-only nil)
              (erase-buffer)
              (insert doc)
              (setq buffer-read-only t))
            (display-buffer (get-buffer "*tide-doc*"))))))))

(defun mh/eldoc-toggle-buffer ()
  "Toggle the documentation buffer for symbol at point."
  (interactive)
  (if (bound-and-true-p tide-mode)
      (mh/tide-doc-toggle-buffer)
    (let* ((buf (eldoc-doc-buffer))
           (win (and buf (get-buffer-window buf))))
      (if win
          (quit-window nil win)
        (progn
          (unless (bound-and-true-p eldoc-mode)
            (eldoc-mode 1))
          (eldoc-doc-buffer))))))

(global-set-key (kbd "C-c C-.") #'mh/xref-find-definitions-buffer)

(defvar mh/xref-last-marker nil
  "Marker for the last location before jumping to a definition.")

(defun mh/xref-toggle-definitions ()
  "Go to definition, or jump back to the last location."
  (interactive)
  (if (and (markerp mh/xref-last-marker)
           (marker-buffer mh/xref-last-marker)
           (or (not (eq (current-buffer) (marker-buffer mh/xref-last-marker)))
               (/= (point) (marker-position mh/xref-last-marker))))
      (progn
        (switch-to-buffer (marker-buffer mh/xref-last-marker))
        (goto-char (marker-position mh/xref-last-marker))
        (setq mh/xref-last-marker nil))
    (setq mh/xref-last-marker (copy-marker (point) t))
    (xref-find-definitions (thing-at-point 'symbol t))))

(defun mh/xref-clear-marker-on-command ()
  "Clear the xref toggle marker after any non-toggle command."
  (when (and mh/xref-last-marker
             (not (eq this-command 'mh/xref-toggle-definitions)))
    (setq mh/xref-last-marker nil)))

(add-hook 'post-command-hook #'mh/xref-clear-marker-on-command)

(global-set-key (kbd "C-c i") #'mh/eldoc-toggle-buffer)
(global-set-key (kbd "C-d") #'mh/xref-toggle-definitions)
(global-set-key (kbd "M-.") #'mh/xref-toggle-definitions)

(with-eval-after-load 'tide
  (define-key tide-mode-map (kbd "M-.") #'mh/xref-toggle-definitions)
  (define-key tide-mode-map (kbd "C-d") #'mh/xref-toggle-definitions))

(defun mh/tangle-and-restart ()
  "Tangle emacs.org and restart Emacs."
  (interactive)
  (org-babel-tangle-file (expand-file-name "emacs.org" user-emacs-directory))
  (restart-emacs))

(global-set-key (kbd "C-c e r") #'mh/tangle-and-restart)

(require 'claude-chant)
(require 'chant-abeyance)
(require 'chant-dashboard)
;; (require 'chant-docker)

(load "local.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "elisp-editing.el")
(load "clojure.el")
(load "jsts.el")
(load "elixir.el")
(load "dart.el")
(load "vue.el")
(load "web.el")
(load "external-services.el")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
