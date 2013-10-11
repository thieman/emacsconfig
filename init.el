; os x stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

; melpa
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; stick backup files in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default tab-width 4)
(setq whitespace-line-column 80)
(setq require-final-newline 't)
(defalias 'yes-or-no-p 'y-or-n-p)

; key bindings
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-x j") 'ace-jump-mode)

(setq initial-scratch-message "")

; fullscreen on ubuntu
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

; custom function defs
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

; custom standard hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; terminal stuff
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output t)
 '(custom-enabled-themes (quote (hickey)))
 '(custom-safe-themes
   (quote
	("978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" default)))
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(ansi-color-for-comint-mode-on)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(add-to-list 'load-path "~/.emacs.d/vendor/bash-completion")
;; (autoload 'bash-completion-dynamic-complete
;;   "bash-completion"
;;   "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions
;;   'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions
;;   'bash-completion-dynamic-complete)

; additional language modes
; yaml mode
(add-to-list 'load-path "~/.emacs.d/vendor/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
; coffee-script mode
(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)
; handlebars-mode
(add-to-list 'load-path "~/.emacs.d/vendor/handlebars-mode")
(require 'handlebars-mode)
; js2
(add-to-list 'load-path "~/.emacs.d/vendor/javascript")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload 'js2-mode "js2-mode" nil t)
; jinja2
(add-to-list 'load-path "~/.emacs.d/vendor/jinja2")
(require 'jinja2-mode)
(autoload 'jinja2-mode "jinja2" nil t)
(add-to-list 'auto-mode-alist '("\\.html$" . jinja2-mode))
; haskell
(load "~/.emacs.d/vendor/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(set-face-attribute 'default nil :family "Source Code Pro")

(ido-mode)

; epylint for flymake
(setq epylint "/usr/local/bin/epylint")
(when (load "flymake" t)
  (defun flymake-pylint-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
					   'flymake-create-temp-inplace))
		   (local-file (file-relative-name
						temp-file
						(file-name-directory buffer-file-name))))
	  (list epylint (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
			   '("\\.py\\'" flymake-pylint-init)))

; uncomment to enable flymake-mode on python file load
; (add-hook 'find-file-hook 'flymake-find-file-hook)

; fix for env in gui emacs
(defun setenv-from-shell (varname)
  (setenv varname (replace-regexp-in-string
				   "[ \t\n]*$"
				   ""
				   (shell-command-to-string (concat "$SHELL --login -i -c 'echo $" varname "'")))))
(setenv-from-shell "PYTHONPATH")
(setenv-from-shell "PATH")
(setenv-from-shell "TMPDIR")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
