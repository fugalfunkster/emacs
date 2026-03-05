;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(use-package

    ;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit
    
    ;;;;
    ;; FUGALFUNKSTER
    ;;;;

    ;; org mode
    org
    org-beautify-theme
    org-bullets
    ob-typescript
    ob-elixir
  
    ;; JavaScript modes
    js2-mode
    js2-refactor
    js-comint
    nodejs-repl
    tern
    web-mode

    ;; TypeScript
    typescript-mode
    tide
    
    ;; Dart & Flutter
    lsp-mode
    lsp-dart
    lsp-ui
    hover

    ;; syntax and style
    flycheck
    yasnippet
    auto-complete
    company

    ;; shell and dir nav
    direnv
    multi-eshell
    neotree


    ;; tricks
    avy
    ace-window
    emmet-mode
    expand-region
    multiple-cursors
    undo-tree
    
))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;;
;; Customization
;;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Xref: keep minibuffer by default, offer explicit buffer view
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

(add-to-list 'load-path "~/.emacs.d/lisp")
;; Claude prompt composition helpers.
(require 'claude-chant)
(require 'chant-abeyance)
(require 'chant-dashboard)
;; (require 'chant-docker)

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "local.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "clojure.el")
(load "jsts.el")
(load "elixir.el")
(load "dart.el")
(load "vue.el")

;; Web stuff
(load "web.el")

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
