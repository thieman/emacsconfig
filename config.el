; confirm on kill
(setq confirm-kill-emacs 'yes-or-no-p)

; theme
(add-hook 'after-init-hook (lambda () (load-theme 'hickey t)))

; os x stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)

; stick backup files in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;  https://www.emacswiki.org/emacs/YesOrNoP#toc1
(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message "")

;; when you start typing after having marked a region, delete that region
;; and replace with what you're typing
(pending-delete-mode 1)

;; don't confirm creation when switching to a non-existant buffer
(setq ido-create-new-buffer 'always)

;; uniquify adds more information to the status bar when buffers share names
;; e.g. instead of project.clj<2>, you get project.clj@my-project
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "@")

; https://www.emacswiki.org/emacs/AnsiColor
(ansi-color-for-comint-mode-on)

;; Prevent annoying \"Active processes exist\" query when you quit Emacs.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

;; save buffers when changing windows or buffers
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; Support for mega widescreen monitor
;; These vars decide which way Emacs will split a buffer
(setq split-height-threshold 1000)
(setq split-width-threshold 1000)
(set-face-attribute 'default nil :height 110)
