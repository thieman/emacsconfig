(add-hook 'after-init-hook
       (lambda ()
         (require 'flx-ido)
         (ido-mode t)
         (ido-everywhere t)
         (flx-ido-mode t)
         (setq ido-use-faces nil)))
