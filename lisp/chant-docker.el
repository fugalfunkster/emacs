;;; chant-docker.el --- Docker integration for chant agent pairs -*- lexical-binding: t; -*-

;; When loaded, this module replaces chant-dashboard-new-pair with a
;; Docker-aware version that runs each agent inside an isolated container.
;;
;; Load after chant-dashboard in init.el:
;;
;;   (require 'chant-docker)
;;
;; To revert to bare-metal mode, comment out or remove that require and
;; restart Emacs. No other files are modified.

(require 'chant-dashboard)

;;
;; Customization
;;

(defcustom chant-docker-image "chant:latest"
  "Docker image tag to use for agent containers."
  :type 'string
  :group 'claude-chant)

(defcustom chant-docker-agent-script "chant-agent"
  "Shell command used to launch a Docker agent container.
Must be on PATH. Accepts a single argument: the agent pair name."
  :type 'string
  :group 'claude-chant)

;;
;; Internal helpers
;;

(defun chant-docker--image-exists-p ()
  "Return non-nil if `chant-docker-image' is available locally."
  (eq 0 (call-process "docker" nil nil nil
                      "image" "inspect" chant-docker-image)))

(defun chant-docker--agent-command (name)
  "Return the shell command string that launches a Docker agent named NAME."
  (format "%s %s" chant-docker-agent-script (shell-quote-argument name)))

;;
;; Docker-aware replacement for chant-dashboard-new-pair
;;

(defun chant-docker--new-pair (name)
  "Docker-aware version of `chant-dashboard-new-pair'.

Creates a tmux window in the claude session and starts a Docker container
running Claude Code inside it. The container is named chant-NAME.

Replaces the bare `claude' invocation with `chant-agent NAME' so that
every agent runs inside an isolated Docker sandbox."
  (interactive "sAgent name: ")
  (when (string-empty-p (string-trim name))
    (user-error "Agent name cannot be empty"))
  (when (assoc name chant-dashboard--pairs)
    (user-error "Pair \"%s\" already exists" name))
  (when (member name (chant-dashboard--window-list))
    (user-error "tmux window \"%s\" already exists in session \"%s\""
                name chant-dashboard-session))
  ;; Warn if image is not available so the user can build it before the
  ;; container fails silently in the tmux window.
  (unless (chant-docker--image-exists-p)
    (if (yes-or-no-p
         (format "Docker image \"%s\" not found. Continue anyway? " chant-docker-image))
        (message "Continuing — the container may fail to start until the image is built.")
      (user-error "Aborted. Build the image with: cd ~/Code/Roost && docker build -t %s -f Dockerfile.chant ."
                  chant-docker-image)))
  ;; Ensure the tmux session exists
  (unless (chant-dashboard--session-exists-p)
    (chant-dashboard--tmux "new-session" "-d" "-s" chant-dashboard-session))
  ;; Create window and launch Docker agent
  (let ((target (format "%s:%s" chant-dashboard-session name)))
    (chant-dashboard--tmux "new-window" "-t" chant-dashboard-session "-n" name)
    (chant-dashboard--tmux "send-keys" "-t" target
                           (chant-docker--agent-command name) "Enter")
    ;; Register pair (pane-id resolved on next poll if not immediately available)
    (push (cons name (list :window target :status 'active
                           :pane-id (chant-dashboard--pane-id-for name)))
          chant-dashboard--pairs)
    ;; Auto-select if this is the first pair
    (when (null chant-dashboard--active-pair)
      (chant-dashboard-select-pair name))
    (chant-dashboard--render)
    (message "Spawned Docker agent pair: %s (container: chant-%s)" name name)))

;;
;; Activate the override
;;

(advice-add 'chant-dashboard-new-pair :override #'chant-docker--new-pair)

(provide 'chant-docker)

;;; chant-docker.el ends here
