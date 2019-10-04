(setq user-packages
  '(ace-jump-mode
    ag
    cherry-blossom-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    company
    discover-my-major
    dockerfile-mode
    exec-path-from-shell
    flx-ido
    go-mode
    go-projectile
    helm
    helm-projectile
    jedi
    jinja2-mode
    js-doc
    js2-mode
    less-css-mode
    lua-mode
    magit
    markdown-mode
    multiple-cursors
    powerline
    projectile
    rainbow-delimiters
    rainbow-mode
    smex
    sublime-themes
    terraform-mode
    tide
    undo-tree
    visual-regexp
    visual-regexp-steroids
    wsd-mode
    yaml-mode))

(defun thieman-install-packages ()
  "Install any missing packages from the package list."
  (interactive)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (setq packages-refreshed nil)
  (dolist (p user-packages)
    (when (not (package-installed-p p))
      (when (not packages-refreshed)
        (package-refresh-contents)
        (setq packages-refreshed 't))
      (package-install p))))
