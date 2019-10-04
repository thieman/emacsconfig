;; Make Emacs use the same $PATH as our normal user session
(when (memq window-system '(mac ns))
  (add-hook 'after-init-hook
            (lambda ()
              (exec-path-from-shell-initialize)
              (exec-path-from-shell-copy-env "GOPATH")
              (exec-path-from-shell-copy-env "PYTHONPATH"))))
