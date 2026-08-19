;;;;
;; Packages
;;;;

;; Suppress GC and remote-file handlers during startup for faster load.
;; Both are restored after init completes.
(setq gc-cons-threshold most-positive-fixnum)
(defvar my--saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))
            (setq file-name-handler-alist my--saved-file-name-handler-alist)))

(defconst my-local-emacs-dir (expand-file-name "~/.emacs.d-local/"))
(unless (file-directory-p my-local-emacs-dir)
  (make-directory my-local-emacs-dir t))
(setq package-user-dir (expand-file-name "elpa/" my-local-emacs-dir))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(use-package
    exec-path-from-shell
    ido-completing-read+
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
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
    web-mode
    typescript-mode
    lsp-mode
    lsp-dart
    lsp-treemacs
    lsp-ui
    hover
    flutter
    prettier-js
    flycheck
    flycheck-eglot
    keyfreq
    yasnippet
    auto-complete
    company
    direnv
    neotree
    avy
    ace-window
    emmet-mode
    expand-region
    multiple-cursors
    undo-tree
    restart-emacs
    ox-pandoc))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Pull PATH from the login shell so GUI Emacs sees the same tools as the
;; terminal. Must run after the install loop so a fresh machine can bootstrap.
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/lisp")

(defun mh/xref-find-definitions-buffer ()
  "Show xref results for symbol at point in a separate buffer."
  (interactive)
  (let ((xref-show-xrefs-function #'xref-show-definitions-buffer)
        (xref-show-definitions-function #'xref-show-definitions-buffer))
    (call-interactively #'xref-find-definitions)))

(defun mh/eldoc-toggle-buffer ()
  "Toggle the documentation buffer for symbol at point."
  (interactive)
  (let* ((buf (get-buffer "*lsp-help*"))
         (win (and buf (get-buffer-window buf))))
    (cond
     (win (quit-window nil win))
     ((bound-and-true-p lsp-mode)
      (lsp-describe-thing-at-point))
     (t
      (unless (bound-and-true-p eldoc-mode) (eldoc-mode 1))
      (eldoc-doc-buffer)))))

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

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "M-.") #'mh/xref-toggle-definitions)
  (define-key lsp-mode-map (kbd "C-d") #'mh/xref-toggle-definitions))

(defun tangle-and-restart ()
  "Tangle all .org files in user-emacs-directory and restart Emacs.
Deletes stale .elc files in lisp/ before tangling so the freshly
tangled .el files are always loaded on restart."
  (interactive)
  (dolist (elc (directory-files
                (expand-file-name "lisp/" user-emacs-directory) t "\\.elc$"))
    (delete-file elc))
  (dolist (file (directory-files user-emacs-directory t "\\.org$"))
    (org-babel-tangle-file file))
  (restart-emacs))

(defun tangle-and-reload ()
  "Tangle all .org files and hot-reload everything without restarting.
Deletes stale .elc files first so the freshly tangled .el files are loaded.
Reloads both customizations/ modules and lisp/ bebop modules in dependency order.
Callable via emacsclient: emacsclient --eval \"(tangle-and-reload)\""
  (interactive)
  (dolist (dir (list (expand-file-name "lisp/" user-emacs-directory)
                     (expand-file-name "customizations/" user-emacs-directory)))
    (when (file-directory-p dir)
      (dolist (elc (directory-files dir t "\\.elc$"))
        (delete-file elc))))
  (dolist (file (directory-files user-emacs-directory t "\\.org$"))
    (org-babel-tangle-file file))
  ;; Reload customizations/ modules
  (dolist (f '("local-state.el" "navigation.el" "ui.el" "editing.el"
               "misc.el" "elisp-editing.el" "clojure.el" "lsp-common.el"
               "jsts.el" "dart.el" "vue.el" "web.el" "external-services.el"))
    (load f 'noerror 'nomessage))
  ;; Reload lisp/ bebop modules in dependency order
  (let ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
    (dolist (f '("bebop-core.el" "bebop-passthrough.el" "bebop-dashboard.el"
                 "bebop-session.el" "bebop-backline.el" "bebop-set.el"
                 "bebop-api.el" "bebop-call.el" "bebop-frame.el"))
      (load (expand-file-name f lisp-dir) 'noerror 'nomessage)))
  ;; Re-apply Conductor frame-local face settings (fringe, borders, dividers)
  ;; since module reloads reset them.
  (when (fboundp 'bebop-frame--setup-conductor)
    (let ((conductor (cl-find-if (lambda (f) (frame-parameter f 'bebop-conductor))
                                 (frame-list))))
      (when conductor
        (bebop-frame--setup-conductor conductor))))
  (message "tangle-and-reload complete"))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'bebop-core)
(require 'bebop-passthrough)
(require 'bebop-dashboard)
(require 'bebop-session)
(require 'bebop-backline)
(require 'bebop-set)
(require 'bebop-api)
(require 'bebop-call)
(require 'bebop-frame)

;; Answer cross-machine calls when a Dropbox mailbox is available.
(when bebop-call-dir
  (bebop-call-mode 1))

(load "local-state.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "elisp-editing.el")
(load "clojure.el")
(load "lsp-common.el")
(load "jsts.el")
(load "dart.el")
(load "vue.el")
(load "web.el")
(load "external-services.el")

(load custom-file 'noerror)
