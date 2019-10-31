(setq-default typescript-indent-level 2)

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (local-set-key (kbd "C-c .") 'tide-jump-to-definition)
            (setq flycheck-checker 'javascript-eslint)
            (setq flycheck-eslint-args '("--ext" ".js,.ts"))))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2))
(put 'upcase-region 'disabled nil)
