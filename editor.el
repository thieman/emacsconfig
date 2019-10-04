(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ; seriously, fuck tabs

(setq whitespace-line-column 80)

(setq require-final-newline 't)

(global-auto-revert-mode)

(show-paren-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
