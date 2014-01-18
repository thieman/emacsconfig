(setq user-packages
  '(ace-jump-mode
    ag
    auto-complete
    cider
    circe
    clojure-cheatsheet
    clojure-mode
    coffee-mode
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized
    dired+
    handlebars-mode
    jedi
    jinja2-mode
    js2-mode
    less-css-mode
    magit
    minimap
    powerline
    rainbow-delimiters
    smex
    soundcloud
    sublime-themes
    yaml-mode))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq packages-refreshed nil)
(dolist (p user-packages)
  (when (not (package-installed-p p))
    (when (not packages-refreshed)
      (package-refresh-contents)
      (setq packages-refreshed 't))
    (package-install p)))

(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

; os x stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

; emms
(setq exec-path (append exec-path '("/usr/local/bin")))
(require 'emms-setup)
(emms-standard)
(emms-default-players)

; stick backup files in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default tab-width 4)
(setq whitespace-line-column 80)
(setq require-final-newline 't)
(setq-default indent-tabs-mode nil)  ; seriously, fuck tabs
(defalias 'yes-or-no-p 'y-or-n-p)

; key bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-x j") 'ace-jump-mode)
(global-set-key (kbd "M-s") 'soundcloud)
(global-set-key (kbd "C-x g") 'ag-project)

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

;; when you start typing after having marked a region, delete that region
;; and replace with what you're typing
(pending-delete-mode 1)

;; don't confirm creation when switching to a non-existant buffer
(setq ido-create-new-buffer 'always)

;;-- init.nav.uniquify
;; uniquify adds more information to the status bar when buffers share names
;; e.g. instead of project.clj<2>, you get project.clj@my-project
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "@")

(show-paren-mode 1)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode-enable)
(after 'clojure-mode (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode-enable))

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
 '(custom-safe-themes (quote ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" default)))
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

(ido-mode)

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
(put 'erase-buffer 'disabled nil)

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; rope and pymacs
(after 'auto-complete
       (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
       (setq ac-use-menu-map t)
       (define-key ac-menu-map "\C-n" 'ac-next)
       (define-key ac-menu-map "\C-p" 'ac-previous))

(after 'auto-complete-config
       (ac-config-default)
       (when (file-exists-p (expand-file-name "/Users/dcurtis/.emacs.d/elisp/Pymacs"))
         (ac-ropemacs-initialize)
         (ac-ropemacs-setup)))

(after 'auto-complete-autoloads
       (autoload 'auto-complete-mode "auto-complete" "enable auto-complete-mode" t nil)
       (add-hook 'python-mode-hook
                 (lambda ()
                   (require 'auto-complete-config)
                   (add-to-list 'ac-sources 'ac-source-ropemacs)
                   (auto-complete-mode))))

;; pymacs
(add-to-list 'load-path "~/.emacs.d/elisp")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; ag
(setq ag-highlight-search 't)
(setq ag-reuse-buffers 't)
(setq ag-reuse-window 't)

;; powerline
(powerline-default-theme)

;; dired+
(toggle-diredp-find-file-reuse-dir 1)

;; circe
(setq circe-reduce-lurker-spam t)
(setq circe-serve-max-reconnect-attempts nil)
(after 'circe
       (require 'circe-color-nicks)
       (enable-circle-color-nicks))
