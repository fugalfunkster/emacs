# Emacs Config — Claude Instructions

## Source of Truth

This is a **literate config**. All `.el` files are generated from `emacs.org`
via `org-babel-tangle`. **Never edit `.el` files directly** — changes will be
overwritten the next time the config is tangled.

To make a config change:
1. Edit the relevant src block in `emacs.org`
2. Run `M-x tangle-and-restart` in Emacs (tangles and restarts in one step)
   — or tangle only with `C-c C-v t` / `M-x org-babel-tangle`

## File Layout

| File/Dir | Purpose |
|----------|---------|
| `emacs.org` | Source of truth — edit this |
| `init.el` | Tangled from emacs.org — do not edit |
| `lisp/*.el` | Tangled from emacs.org — do not edit |
| `customizations/*.el` | Tangled from emacs.org — do not edit |
| `custom.el` | Written by `M-x customize` — intentionally excluded from tangle |
| `emacsHelp.org` | Keybindings reference |

## Module Section Pattern

Each `.el` module in `lisp/` corresponds to a level-2 section in `emacs.org`
with a `:tangle` PROPERTIES drawer. All `#+begin_src` blocks within the section
inherit the tangle target automatically — do not repeat `:tangle` on individual
blocks.

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
