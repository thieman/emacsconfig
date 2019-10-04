(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(setq custom-file "~/emacs-custom.el")
(load-user-file "emacs-custom.el")

;; Stuff we need in the global namespace
(load-user-file "lib.el")
(load-user-file "functions.el")

;; Stuff related to Emacs itself and first party settings
(load-user-file "config.el")
(load-user-file "editor.el")
(load-user-file "keybindings.el")
(load-user-file "packages.el")

;; OS-specific configs
(load-user-file "mac.el")

;; Third party extensions other than language extensions
(load-user-file "ag.el")
(load-user-file "company.el")
(load-user-file "flx-ido.el")
(load-user-file "misc-extensions.el")
(load-user-file "powerline.el")
(load-user-file "projectile.el")

;; Language configs
(load-user-file "go.el")
(load-user-file "html.el")
(load-user-file "javascript.el")
(load-user-file "misc-languages.el")
(load-user-file "python.el")
(load-user-file "typescript.el")
