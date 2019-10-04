;; (defun default-projectile-prefix-unless-django (project-type)
;;   (when (not (eq project-type 'django))
;;     (projectile-test-prefix project-type)))

;; (defun default-projectile-suffix-unless-django (project-type)
;;   (if (eq project-type 'django)
;;       "_test"
;;     (projectile-test-suffix project-type)))

(add-hook 'after-init-hook
          (lambda ()
            (projectile-global-mode)
            (defun projectile-test-prefix (project-type) nil)
            (defun projectile-test-suffix (project-type)
              (cond
               ((member project-type '(rails-rspec ruby-rspec)) "_spec")
               ((member project-type '(rails-test ruby-test lein django python)) "_test")
               ((member project-type '(maven symfony)) "Test")))))
