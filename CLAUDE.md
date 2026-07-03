# Emacs Config — Claude Instructions

## Source of Truth

This is a **literate config**. All `.el` files are generated from `emacs.org`
and `bebop.org` via `org-babel-tangle`. **Never edit `.el` files directly** —
changes will be overwritten the next time the config is tangled.

The org structure is documentation, not scaffolding. When adding or changing
code: find the appropriate existing section rather than appending to the end,
and include a brief prose explanation alongside new code blocks. Headings become
comments in the tangled output (via `:comments org`), so a well-placed heading
with clear prose is the correct way to explain intent.

To make a config change:
1. Edit the relevant src block in `emacs.org` or `bebop.org`
2. **Tangle and validate before loading** — paren errors in elisp src blocks are
   easy to introduce and hard to spot by eye. Always validate after tangling:
   ```bash
   # Tangle
   emacsclient --eval "(org-babel-tangle-file \"/Users/roostuser/Dropbox/.emacs.d/bebop.org\")"
   # Validate each changed .el file (repeat per file)
   /Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "
   (with-temp-buffer
     (insert-file-contents \"~/.emacs.d/lisp/bebop-session.el\")
     (emacs-lisp-mode)
     (condition-case err
         (progn (check-parens) (message \"OK\"))
       (error (message \"line %d col %d: %s\"
                       (line-number-at-pos) (current-column)
                       (error-message-string err)))))"
   ```
   Only load if validation passes. This catches unbalanced parens before they
   break the running Emacs.
3. Apply the change — prefer hot-reload over restart:
   - **Hot-reload** (preferred): `emacsclient --eval "(tangle-and-reload)"` — tangles
     all `.org` files, deletes stale `.elc` files, and reloads all customization
     modules (`customizations/`) and all Bebop modules (`lisp/`) in dependency order,
     without restarting. Requires the Emacs server (started automatically via
     `(server-start)` in `init.el`).
   - **Full restart**: `M-x tangle-and-restart` — only needed when adding new
     packages to `my-packages` or changing `init.el` load order.
   - **Tangle only**: `C-c C-v t` / `M-x org-babel-tangle` — when you just want
     the `.el` files without loading them.

## File Layout

| File/Dir | Purpose |
|----------|---------|
| `emacs.org` | General config source of truth — edit this |
| `bebop.org` | Bebop system source of truth — edit this |
| `init.el` | Tangled from emacs.org — do not edit |
| `lisp/*.el` | Bebop modules, tangled from bebop.org — do not edit |
| `customizations/*.el` | General config modules, tangled from emacs.org — do not edit |
| `custom.el` | Written by `M-x customize` — intentionally excluded from tangle |
| `emacsHelp.org` | Keybindings reference |

`lisp/` is for Bebop's structured package modules. `customizations/` is for
flat config files (UI, editing, language modes, etc.).

## Bebop

Bebop is an AI agent orchestration system built in Emacs. It manages Claude Code
sessions running in tmux windows, with a context model built on org-mode. Full
design spec and vocabulary are in `bebop.org` — read that first for any non-trivial work.

**Module map** (`lisp/` — all tangled from `bebop.org`):

| Module | Responsibility |
|--------|---------------|
| `bebop-core.el` | Shared primitives: tmux send pattern, buffer/face setup, eshell hook |
| `bebop-passthrough.el` | Keyboard passthrough to agent's tmux pane (`M-a`); handles TUI dialogs |
| `bebop-dashboard.el` | `*bebop*` buffer — session pool, status dots, lifecycle commands |
| `bebop-session.el` | Session creation/resume/kill/archive; chart and venue management; JIRA.org overlays (`bebop-jira-apply-overlays`, hooks on `org-mode-hook` + `after-save-hook`) |
| `bebop-cue.el` | Cue/jam commands (`C-c C-p` / `C-c C-j`) routing org headings to charts and agents |
| `bebop-frame.el` | Conductor and Solo frame layouts |
| `bebop-gitlab.el` | GitLab MR sync into org (optional dep — see External Services below) |

**Key entry points** (from `bebop.org` Implementation Notes):
- `bebop-send-buffer` — the tmux send pattern all jam/composition sends use
- `bebop--active-session` — `defvar` (not a function) holding the active session name
- `bebop-select-session` — call this to change the active session; propagates everywhere

## Agent Skills

Two Claude skills are available for config work — prefer using them over manual steps:

- **`emacs-eval`** — evaluate elisp in the running Emacs via emacsclient; use to
  verify a change took effect after hot-reload (check mode state, variable values, keybindings)
- **`emacs-qa`** — tangle, run load/byte-compile checks in batch, then hot-reload
  if all checks pass; use when making structural changes or adding new modules

## Module Section Pattern

Each `.el` module in `lisp/` corresponds to a level-2 section in `bebop.org`
with a `:tangle` PROPERTIES drawer. All `#+begin_src` blocks within the section
inherit the tangle target automatically — do not repeat `:tangle` on individual
blocks.

The `customizations/` modules (tangled from `emacs.org`) use a simpler pattern:
a single top-level `#+begin_src emacs-lisp :tangle customizations/X.el` block
per section, without a PROPERTIES drawer.

```org
** Module Name (module-name.el)
   :PROPERTIES:
   :header-args:emacs-lisp: :tangle lisp/module-name.el
   :END:

Prose description of the module.

*** Subsection

#+begin_src emacs-lisp
;; code here — tangled to lisp/module-name.el automatically
#+end_src
```

The file-level property `#+PROPERTY: header-args:emacs-lisp :comments org :mkdirp yes`
applies globally — org headings become comments in the tangled output, and
`lisp/` is created if absent.

To load a module, add a `use-package` block in the `*** Init` section of
`emacs.org`. Commenting out that block and running `tangle-and-restart` fully
disables the module without touching any `.el` file.

## Org Docs

Most org documents (specs, notes, Rodeo config) live in `~/code/org/`.

## External Services (`customizations/external-services.el`)

- **org-jira**: fully configured and working
- **forge**: config block is present but commented out — see TODO comment in
  the file. As of early 2026 there is a magit/transient version incompatibility
  that makes forge unusable. Re-enable when a compatible forge release appears:
  1. Verify `M-x package-install RET forge` compiles without errors
  2. Add `machine gitlab.com login USERNAME^forge password TOKEN` to `~/.authinfo.gpg`
  3. Run `git config --global gitlab.user USERNAME`
  4. Uncomment the forge block in `emacs.org` and run `tangle-and-restart`
  5. Verify with `M-x forge-pull` in a Magit buffer
