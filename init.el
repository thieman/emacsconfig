(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '(("magit" . "melpa-stable"))))

(setq user-packages
  '(ace-jump-mode
    ag
    auto-complete
    cherry-blossom-theme
    cider
    circe
    clojure-cheatsheet
    clojure-mode
    coffee-mode
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized
    dash-at-point
    dired+
    dockerfile-mode
    exec-path-from-shell
    flx-ido
    gh
    go-mode
    go-play
    go-projectile
    handlebars-mode
    helm
    helm-projectile
    jinja2-mode
    js2-mode
    js-doc
    less-css-mode
    lua-mode
    magit
    magit-popup
    markdown-mode
    minimap
    powerline
    projectile
    rainbow-delimiters
    smex
    soundcloud
    sublime-themes
    undo-tree
    wsd-mode
    yaml-mode))

(defun thieman-install-packages ()
  "Install any missing packages from the package list."
  (interactive)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (setq packages-refreshed nil)
  (dolist (p user-packages)
    (when (not (package-installed-p p))
      (when (not packages-refreshed)
        (package-refresh-contents)
        (setq packages-refreshed 't))
      (package-install p))))

(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

(defmacro when-not-arm (&rest body)
  `(when (or (string-match "darwin" (emacs-version))
             (not (string-match "arm" (emacs-version))))
     (progn ,@body)))

; confirm on kill
(setq confirm-kill-emacs 'yes-or-no-p)

; theme
(add-hook 'after-init-hook (lambda () (load-theme 'hickey t)))

; os x stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun file-name-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

; emms
(after 'emms
       (require 'emms-setup)
       (emms-standard)
       (emms-default-players))

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
(setq sgml-basic-offset 2)  ; use 2 space indent in html mode
(setq handlebars-basic-offset 4)
(setq js-indent-level 4)
(add-to-list 'auto-mode-alist '("\\.html\\'" . jinja2-mode))  ;; use jinja2-mode for html files

; key bindings
(when-not-arm (global-set-key (kbd "M-x") 'smex))
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-x j") 'ace-jump-mode)
(global-set-key (kbd "C-x g") 'ag-project)
(global-set-key (kbd "M-p") 'github-pulls)
(global-set-key (kbd "C-c C-p") 'compile)
(global-set-key (kbd "M-s") 'magit-status)

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
    ("298f3826066ad761b9f461eec62b578c9725fd6134fdcab75fef579032f03d34" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bc89fda3d232a3daa4eb3a9f87d6ffe1272fea46e4cf86686d9e8078e4209e2c" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" default)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(js2-strict-inconsistent-return-warning nil)
 '(magit-revert-buffers (quote silent))
 '(projectile-test-prefix-function (quote default-projectile-prefix-unless-django))
 '(projectile-test-suffix-function (quote default-projectile-suffix-unless-django))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(ansi-color-for-comint-mode-on)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun python-custom ()
  "Python mode stuff"
  (define-key python-mode-map (kbd "M-]") 'python-indent-shift-right)
  (define-key python-mode-map (kbd "M-[") 'python-indent-shift-left))
(add-hook 'python-mode-hook '(lambda () (python-custom)))

;; ag
(setq ag-highlight-search 't)
(setq ag-reuse-buffers 't)
(setq ag-reuse-window 't)

;; powerline
(after 'powerline (powerline-default-theme))

;; dired+
(after 'dired (toggle-diredp-find-file-reuse-dir 1))

;; circe
(after 'circe
       (require 'circe-color-nicks)
       (setq circe-reduce-lurker-spam t)
       (setq circe-serve-max-reconnect-attempts nil)
       (enable-circe-color-nicks))

;; projectile
(add-hook 'after-init-hook
          (lambda ()
            (projectile-global-mode)
            (defun projectile-test-prefix (project-type) nil)
            (defun projectile-test-suffix (project-type)
              (cond
               ((member project-type '(rails-rspec ruby-rspec)) "_spec")
               ((member project-type '(rails-test ruby-test lein django python)) "_test")
               ((member project-type '(maven symfony)) "Test")))))

;; flyspell-prog-mode
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'clojure-mode-hook 'flyspell-prog-mode)

(defun default-projectile-prefix-unless-django (project-type)
  (when (not (eq project-type 'django))
    (projectile-test-prefix project-type)))

(defun default-projectile-suffix-unless-django (project-type)
  (if (eq project-type 'django)
      "_test"
    (projectile-test-suffix project-type)))

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (add-hook 'after-init-hook
            (lambda ()
              (exec-path-from-shell-initialize)
              (exec-path-from-shell-copy-env "GOPATH")
              (exec-path-from-shell-copy-env "PYTHONPATH"))))

;; flx-ido
(add-hook 'after-init-hook
       (lambda ()
         (require 'flx-ido)
         (ido-mode t)
         (ido-everywhere t)
         (flx-ido-mode t)
         (setq ido-use-faces nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1D2021" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Source Code Pro for Powerline")))))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; js-doc
(setq js-doc-mail-address "travis.thieman@gmail.com"
      js-doc-author (format "Travis Thieman <%s>" js-doc-mail-address)
      js-doc-url "http://github.com/thieman")
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; save buffers when changing windows or buffers
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; cider stuff
(setq cider-show-error-buffer nil)

;; undo-tree
(after 'undo-tree
       (require 'undo-tree)
       (global-undo-tree-mode t))

;; go-mode
(defun my-go-mode-hook ()
  ; Use goimports instead of gofmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c .") 'godef-jump)
  ; Praise Go Oracle
  (go-oracle-mode))

(after 'go-mode
       (require 'go-projectile)
       (load-file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el"))
(add-hook 'go-mode-hook 'my-go-mode-hook)
