;;; early-init.el --- runs before package activation -*- lexical-binding: t; -*-
;; Emacs runs `package-activate-all' BEFORE loading init.el, using the default
;; package-user-dir (~/.emacs.d/elpa). This machine keeps ~/.emacs.d inside
;; Dropbox (CloudStorage), so that default elpa holds dataless, online-only files
;; that native-comp/file-notify cannot hash — a real startup then dies with
;; "File notification error: hashing failed, .../elpa/....el". Redirect
;; package-user-dir to a machine-local elpa here, before any activation, so
;; nothing under the Dropbox elpa is ever loaded. Local State and Init set the
;; same value again, but by then activation has already happened.
(defconst my-local-emacs-dir (expand-file-name "~/.emacs.d-local/"))
(unless (file-directory-p my-local-emacs-dir)
  (make-directory my-local-emacs-dir t))
(setq package-user-dir (expand-file-name "elpa/" my-local-emacs-dir))
