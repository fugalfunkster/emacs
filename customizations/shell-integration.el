;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
    '("PATH")))


;; literate prog stuff
;; http://rwx.io/blog/2016/03/09/org-with-babel-node-updated/

(setenv "NODE_PATH"
  (concat
    "/Users/fugalfunkster/code/org/node_modules" ":"
    (getenv "NODE_PATH")
  )
)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)))


