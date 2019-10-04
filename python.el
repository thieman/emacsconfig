(add-hook 'python-mode-hook (lambda ()
  (define-key python-mode-map (kbd "M-]") 'python-indent-shift-right)
  (define-key python-mode-map (kbd "M-[") 'python-indent-shift-left)))

;; only spell check comments
(add-hook 'python-mode-hook 'flyspell-prog-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
