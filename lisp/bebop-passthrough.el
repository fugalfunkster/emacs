;;; bebop-passthrough.el --- Live keystroke passthrough for the bebop buffer -*- lexical-binding: t; -*-

;; Usage:
;;   M-a                    toggle passthrough mode on/off in the composition buffer
;;   M-x bebop-passthrough  same, if you prefer
;;
;; In passthrough mode, every keystroke is forwarded directly to the Claude
;; tmux pane as a terminal control sequence. Use it to navigate TUI prompts
;; (selection menus, confirmations, yes/no) without leaving Emacs.
;; The header color shifts to amber as a visual signal.

(require 'bebop-core)

(defcustom bebop-passthrough-color "#4B0082"
  "Header color when passthrough mode is active (royal purple)."
  :type 'string
  :group 'bebop)

(defvar bebop-passthrough--key-table
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

(defun bebop-passthrough--send (key-desc)
  "Send KEY-DESC to the Claude tmux pane."
  (let ((tmux-key (or (cdr (assoc key-desc bebop-passthrough--key-table))
                      ;; Fall back to the raw key description for
                      ;; printable characters and anything not in the table
                      key-desc)))
    (call-process "tmux" nil nil nil
                  "send-keys" "-t" bebop-tmux-target tmux-key)))

(defun bebop-passthrough--forward-key ()
  "Forward the current keystroke to the Claude tmux pane."
  (interactive)
  (bebop-passthrough--send (key-description (this-command-keys))))

(defvar-local bebop-passthrough--header-cookie nil
  "Face-remap cookie for the amber passthrough header-line background.")

(defun bebop-passthrough--activate-header ()
  "Shift the header-line background to amber as a passthrough indicator."
  (unless bebop-passthrough--header-cookie
    (setq bebop-passthrough--header-cookie
          (face-remap-add-relative 'header-line
                                   :background bebop-passthrough-color))))

(defun bebop-passthrough--restore-header ()
  "Restore the header-line background to normal."
  (when bebop-passthrough--header-cookie
    (face-remap-remove-relative bebop-passthrough--header-cookie)
    (setq bebop-passthrough--header-cookie nil)))

(defun bebop-passthrough--send-return-and-exit ()
  "Send Return to the Claude pane and exit passthrough mode."
  (interactive)
  (bebop-passthrough--send "Enter")
  (bebop-passthrough))

(defvar bebop-passthrough-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'bebop-passthrough--forward-key)
    (define-key map (kbd "RET") #'bebop-passthrough--send-return-and-exit)
    (define-key map (kbd "<return>") #'bebop-passthrough--send-return-and-exit)
    (define-key map (kbd "M-a") #'bebop-passthrough)
    map)
  "Keymap active in `bebop-passthrough-mode'.")

(define-minor-mode bebop-passthrough-mode
  "Minor mode that forwards all keystrokes to the Claude tmux pane.
In this mode the composition buffer becomes a live passthrough for TUI
interactions (selection menus, confirmations, yes/no prompts).
Toggle with M-x bebop-passthrough or M-a."
  :lighter " Passthrough"
  :keymap bebop-passthrough-mode-map
  (if bebop-passthrough-mode
      (progn
        (bebop-passthrough--activate-header)
        (message "Passthrough — keystrokes forwarded to Claude"))
    (progn
      (bebop-passthrough--restore-header)
      (message "Passthrough off"))))

(defun bebop-passthrough ()
  "Toggle passthrough mode in the current Bebop composition buffer.
When active, all keystrokes are forwarded directly to the Claude tmux pane.
The header color shifts to amber as a visual signal.
Toggle with M-a."
  (interactive)
  (let ((buf (if (string-prefix-p "*bebop-session:" (buffer-name))
                 (current-buffer)
               (get-buffer-create bebop-composition-buffer-name))))
    (unless (eq (current-buffer) buf)
      (switch-to-buffer buf))
    (bebop-passthrough-mode (if bebop-passthrough-mode -1 1))))

(advice-add 'bebop-open-composition-buffer :after
            (lambda (&rest _)
              (local-set-key (kbd "M-a") #'bebop-passthrough)))

(provide 'bebop-passthrough)

;;; bebop-passthrough.el ends here
