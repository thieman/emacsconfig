;; undo-tree
(thieman-after 'undo-tree
       (require 'undo-tree)
       (global-undo-tree-mode t))

; emms
(thieman-after 'emms
       (require 'emms-setup)
       (emms-standard)
       (emms-default-players))

;; visual-regexp
(add-hook 'after-init-hook
          (lambda ()
            (require 'visual-regexp)))
