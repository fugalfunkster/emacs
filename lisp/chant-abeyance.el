;;; chant-abeyance.el --- Live keystroke passthrough for the chant buffer -*- lexical-binding: t; -*-

;; Usage:
;;   M-a                 toggle abeyance mode on/off in the *chant* buffer
;;   M-x abeyance        same, if you prefer
;;
;; In abeyance mode, every keystroke is forwarded directly to the Claude
;; tmux pane as a terminal control sequence. Use it to navigate TUI prompts
;; (selection menus, confirmations, yes/no) without leaving Emacs.
;; The chant header changes to signal the active mode.

(require 'claude-chant)

(defcustom chant-abeyance-header-text "Abeyance"
  "Header text shown in the chant buffer when abeyance mode is active."
  :type 'string
  :group 'claude-chant)

(defcustom chant-abeyance-header-color "#D4A017"
  "Header color when abeyance mode is active (amber)."
  :type 'string
  :group 'claude-chant)

(defvar chant-abeyance--key-table
  '(;; Arrow keys
    ("<up>"      . "Up")
    ("<down>"    . "Down")
    ("<left>"    . "Left")
    ("<right>"   . "Right")
    ;; Navigation
    ("<return>"  . "Enter")
    ("RET"       . "Enter")
    ("<escape>"  . "Escape")
    ("ESC"       . "Escape")
    ("<tab>"     . "Tab")
    ("TAB"       . "Tab")
    ("SPC"       . "Space")
    ("<backtab>" . "BTab")       ; Shift-Tab
    ("<backspace>" . "BSpace")
    ("DEL"       . "BSpace")
    ("<delete>"  . "DC")
    ("<home>"    . "Home")
    ("<end>"     . "End")
    ("<prior>"   . "PPage")      ; Page Up
    ("<next>"    . "NPage")      ; Page Down
    ;; Common control sequences
    ("C-a"  . "C-a")
    ("C-b"  . "C-b")
    ("C-c"  . "C-c")
    ("C-d"  . "C-d")
    ("C-e"  . "C-e")
    ("C-f"  . "C-f")
    ("C-g"  . "C-g")
    ("C-h"  . "C-h")
    ("C-k"  . "C-k")
    ("C-l"  . "C-l")
    ("C-n"  . "C-n")
    ("C-p"  . "C-p")
    ("C-r"  . "C-r")
    ("C-s"  . "C-s")
    ("C-t"  . "C-t")
    ("C-u"  . "C-u")
    ("C-w"  . "C-w")
    ("C-y"  . "C-y")
    ("C-z"  . "C-z"))
  "Alist mapping Emacs key descriptions to tmux send-keys names.")

(defun chant-abeyance--send (key-desc)
  "Send KEY-DESC to the Claude tmux pane."
  (let ((tmux-key (or (cdr (assoc key-desc chant-abeyance--key-table))
                      ;; Fall back to the raw key description for
                      ;; printable characters and anything not in the table
                      key-desc)))
    (call-process "tmux" nil nil nil
                  "send-keys" "-t" claude-chant-target tmux-key)))

(defun chant-abeyance--forward-key ()
  "Forward the current keystroke to the Claude tmux pane."
  (interactive)
  (chant-abeyance--send (key-description (this-command-keys))))

(defun chant-abeyance--activate-header ()
  "Switch the chant buffer header to abeyance style."
  (with-current-buffer (get-buffer-create claude-chant-buffer-name)
    (setq claude-chant--header-mode chant-abeyance-header-text)
    (let ((claude-chant-header-color chant-abeyance-header-color))
      (claude-chant--set-header))))

(defun chant-abeyance--restore-header ()
  "Restore the chant buffer header to normal style."
  (with-current-buffer (get-buffer-create claude-chant-buffer-name)
    (setq claude-chant--header-mode "Chant")
    (claude-chant--set-header)))

(defun chant-abeyance--send-return-and-exit ()
  "Send Return to the Claude pane and exit abeyance mode."
  (interactive)
  (chant-abeyance--send "Enter")
  (abeyance))

(defvar chant-abeyance-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'chant-abeyance--forward-key)
    (define-key map (kbd "RET") #'chant-abeyance--send-return-and-exit)
    (define-key map (kbd "<return>") #'chant-abeyance--send-return-and-exit)
    (define-key map (kbd "M-a") #'abeyance)
    map)
  "Keymap active in `chant-abeyance-mode'.")

(define-minor-mode chant-abeyance-mode
  "Minor mode that forwards all keystrokes to the Claude tmux pane.
In this mode the *chant* buffer becomes a live passthrough for TUI
interactions. Toggle with M-x abeyance."
  :lighter " Abeyance"
  :keymap chant-abeyance-mode-map
  (if chant-abeyance-mode
      (progn
        (chant-abeyance--activate-header)
        (message "Abeyance — keystrokes forwarded to Claude"))
    (progn
      (chant-abeyance--restore-header)
      (message "Abeyance off — composition mode restored"))))

(defun abeyance ()
  "Toggle abeyance mode in the *chant* buffer.
When active, all keystrokes are forwarded directly to the Claude tmux pane."
  (interactive)
  (let ((buf (get-buffer-create claude-chant-buffer-name)))
    (unless (eq (current-buffer) buf)
      (switch-to-buffer buf))
    (chant-abeyance-mode (if chant-abeyance-mode -1 1))))

(advice-add 'claude-chant-open-buffer :after
            (lambda (&rest _)
              (local-set-key (kbd "M-a") #'abeyance)))

(provide 'chant-abeyance)

;;; chant-abeyance.el ends here
