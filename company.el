(add-hook 'after-init-hook
          (lambda ()
            (setq company-idle-delay 0.1)
            (global-company-mode)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
