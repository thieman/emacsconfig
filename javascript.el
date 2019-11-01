(setq js-indent-level 2)

;; use js2-mode for js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; js-doc, in case we ever use that again?
(setq js-doc-mail-address "travis.thieman@gmail.com"
      js-doc-author (format "Travis Thieman <%s>" js-doc-mail-address)
      js-doc-url "http://github.com/thieman")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (flycheck-mode +1)
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))
