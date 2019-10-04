;; indent that applies to Emacs' built in html modes
;; jinja2 might piggy-back on this?
(setq sgml-basic-offset 4)

;; use jinja2-mode for html files
(add-to-list 'auto-mode-alist '("\\.html\\'" . jinja2-mode))
