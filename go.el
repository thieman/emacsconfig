;; go-mode
(defun my-go-mode-hook ()
  ;; call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c .") 'godef-jump))

(thieman-after 'go-mode
               (require 'go-projectile))

(add-hook 'go-mode-hook 'my-go-mode-hook)
