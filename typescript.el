(setq-default typescript-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (local-set-key (kbd "C-c .") 'tide-jump-to-definition)
            (setq flycheck-checker 'typescript-tide)
            (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
            (setq flycheck-eslint-args '("--ext" ".js,.ts,.jsx,.tsx"))))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2))
(put 'upcase-region 'disabled nil)
