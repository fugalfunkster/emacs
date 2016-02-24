
# My JavaScript-friendly emacs Config 

based on flyingmachines [Clojure-friendly config](https://github.com/flyingmachine/emacs-for-clojure)

and azers [JS config](https://github.com/azer/emacs)

with insight from [Howard Abrams](https://github.com/howardabrams/dot-files)

=======

## Hotkeys and Chords I use a Lot

Emacs Basics
- C-g              (quit command)
- C-x b            (switch buffers, and create new buffer)
- C-x C-f          (open file, and create new file)
- C-x C-s          (save buffer to a file)
- C-x C-w          (save as)
- C-x 0            (remove current window)
- C-x 1            (remove all window but the window containing the point)
- C-x 2            (split current window into two: horizontally)
- C-x 3            (split current window into two: vertically)
- C-x o            (move point into another window)
- M-<              (move point to top of buffer)
- M->              (move point to bottom of buffer)
- C-a              (move point to beginning of line)
- C-e              (move point to end of line)
- C-n              (move to next line)
- C-p              (move to previous line)
- M-b              (move forward one word)
- M-f              (move back one word)
- C-s              (regex search text)
- C-space          (set mark)
- C-w              (kill region)
- C-y              (yank region)
- C-h k <key>      (describe key)

Shell
- M-x eshell       (enter eShell)
- C-u M-x eshell   (new eshell instance)

Neotree
- M-x neotree      (use current dir)
- M-x neotree-dir  (specify dir)
- H                (toggle, show hidden files)
- A                (min/max)
- g                (refresh buffer)
- TAB/RET/SPC      (fold & unfold)

Magit
- s                (stage change at point)
- u                (unstage change at point)
- S                (stage all changes in worktree)
- U                (reset indes to some commit)
- C                (commit)
- C-c              (commit in commit message buffer)
- M                (setup remote repo)
- P                (push)
- F                (pull)
- l                (logging)

JS2-mode
- C-c C-e          (hide element {Ex: function body})
- C-c C-s          (show element)

flycheck
- C-c ! l          (list all errors)
- C-c ! x          (disable flycheck in buffer)
- C-c ! v          (verify flycheck works)

org-mode
- M-RET            (create return at same level)
- M-S-RET          (insert new TODO at same level)
- M-S-arrow        (move, promote, demote current subtree)
- C-c C-t          (cycle todo ring)
- C-c C-n          (move to next heading)
- C-c C-p          (move to previous heading)
- C-c C-f          (move to next heading at the same level)
- C-c C-b          (move to previous heading at the same level)
- C-c C-u          (move to a higher level heading)

(more to come...)

_________________

## Included Packages and Helpful Links

async
- http://elpa.gnu.org/packages/async.html

auto-complete

cider
- http://www.github.com/clojure-emacs/cider

clojure-mode
- http://github.com/clojure-emacs/clojure-mode

clojure-mode-ex
- http://github.com/clojure-emacs/clojure-mode

dash

emmet-mode
- fork of zencoding mode
- https://www.youtube.com/watch?v=p7qore_HpC4
- README: https://github.com/rooney/zencoding/blob/master/README.md
- https://github.com/smihica/emmet-mode

epl
- http://github.com/cask/epl

expand-region
- http://emacsrocks.com/e09.html

exec-path-from-shell
- https://github.com/purcell/exec-path-from-shell

flycheck (jshint jscs)
- https://www.flycheck.org/

git-commit
- https://github.com/magit/magit

ido-completing
- https://github.com/DarwinAwardWinner/ido-ubiquitous

id-ubiquitous
- https://github.com/DarwinAwardWinner/ido-ubiquitous

js-comint
- https://github.com/redguardtoo/js-comint

js2-mode
- https://github.com/mooz/js2-mode/

js2-refactor

magit
- https://github.com/magit/magit

magit-popup
-  https://github.com/magit/magit

multi-eshell
- http://cims.nyu.edu/~stucchio

multiple-cursors
- https://www.youtube.com/watch?v=jNa3axo40qM
- https://www.youtube.com/watch?v=4wvLGJQxEjQ

neotree
- https://github.com/jaypei/emacs-neotree

nodejs-repl
- https://github.com/abicky/nodejs-repl.el 

org

paredit

pkg-info
- https://github.com/lunaryorn/pkg-info.

projectile
- https://github.com/bbatsov/projectile

queue
rainbow-delimiters
- https://github.com/Fanael/rainbow-delimiters

s

seq
- http://elpa.gnu.org/packages/seq.html

smart-forward

smex
- http://github.com/nonsequitur/smex/

spinner
- https://github.com/Malabarba/spinner.el

tagedit

undo-tree
- http://www.dr-qubit.org/emacs.php#undo-tree

with-editor
- https://github.com/magit/with-editor

yasnippet
- https://www.youtube.com/watch?v=-4O-ZYjQxks
- http://github.com/capitaomorte/yasnippet

