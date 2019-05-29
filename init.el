;;; My Personal Emacs Configuration

;;; Author: Tom Arrell <https://github.com/tomarrell>
;;; Inspired by https://sam217pa.github.io/2016/08/30/how-to-make-your-own-spacemacs/

;; Use-package bootstrapping
;;; + adding MELPA package archive
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defconst use-package-verbose t)

(eval-when-compile
  (require 'use-package))

;; Enable Evil scroll up
;; Must be before (require 'evil)
(setq evil-want-C-u-scroll t)

;; =============================
;; ------     Plugins     ------
;; =============================

;; Vim keybindings
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil bindings collection across wider Emacs
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Key binding explanation popup
(use-package which-key :ensure t)

;; Key bindings
(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer my-local-leader-def
    :prefix "SPC m")

  ;; Prevent keymaps from being overridden
  (general-override-mode)

  ;; General bindings
  (my-leader-def
    "SPC" '(execute-extended-command :wk "Command Search")
    "TAB" '(previous-buffer :wk "Previous Buffer"))

  ;; Help bindings
  (my-leader-def
    "h" '(:ignore t :wk "Buffer")
    "hm" '(:wk "Kill"))

  ;; Buffer bindings
  (my-leader-def
    "b" '(:ignore t :wk "Buffer")
    "bk" '(kill-buffer :wk "Kill"))

  ;; Git bindings
  (my-leader-def
    "g" '(:ignore t :wk "Git")
    "gs" '(magit-status :wk "Status"))

  ;; Flycheck error bindings
  (my-leader-def
    "e" '(:ignore t :wk "Error (Flycheck)")
    "ec" '(flycheck-clear :wk "Clear")
    "ev" '(flycheck-verify-setup :wk "Verify Setup")
    "ee" '(flycheck-buffer :wk "Check"))

  ;; Ag bindings
  (my-leader-def
    "s" '(:ignore t :wk "Search")
    "sp" '(counsel-projectile-rg :wk "Grep Project"))

  ;; Project bindings
  (my-leader-def
    "p" '(:ignore t :wk "Project")
    "pp" '(counsel-projectile-switch-project :wk "Switch")
    "pf" '(counsel-projectile-find-file :wk "Find File")
    "pd" '(counsel-projectile-find-dir :wk "Find Dir")
    "pb" '(projectile-discover-projects-in-directory :wk "Discover Projects")))

;; Minibuffer for searching etc
(use-package ivy
  :ensure t
  :config
  ;; Allow up and down in results with Vim keybindings
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

;; Generic set of searching functions
;; Counsel installs Swiper as dependency
(use-package counsel :ensure t)

;; Snippet tooling
(use-package yasnippet :ensure t)

;; Manage projects
(use-package projectile :ensure t)
(use-package counsel-projectile :ensure t)

;; Magit, Git porcelain
(use-package magit :ensure t)
(use-package evil-magit :ensure t)
(use-package git-timemachine :ensure t)

;; JS Syntax Highlighting
(use-package web-mode :ensure t)

;; In buffer completions framework
(use-package company :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Elisp formatter
(use-package elisp-format :ensure t)

;; Rust Language support
(use-package rust-mode :ensure t)
(use-package flymake-rust :ensure t)
(use-package flycheck-rust :ensure t)
(use-package racer :ensure t)
(use-package cargo :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Theme install
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord-light t))

;; =============================
;; ------  Configuration  ------
;; =============================
(require 'which-key)
(require 'evil)
(require 'magit)
(require 'evil-magit)

;; Set where the custom variables are stored
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Open Magit on switch project and remove all other panes
(setq counsel-projectile-switch-project-action
  '(lambda (x)
     (magit-status x)
     (delete-other-windows)
     (message "Opening %s" x)))

;; Sort out ESC being able to quit command
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Fix keymap for changing windows while holding CTRL
(define-key evil-window-map "\C-h" 'evil-window-left)
(define-key evil-window-map "\C-j" 'evil-window-down)
(define-key evil-window-map "\C-k" 'evil-window-up)
(define-key evil-window-map "\C-l" 'evil-window-right)

;; Open maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable the bloody annoying bell
(setq ring-bell-function 'ignore)

;; Enable which key info
(which-key-mode)
(counsel-mode)
(counsel-projectile-mode)
(global-display-line-numbers-mode)

;; Set wrap point
(setq fill-column 120)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Disable the toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable web mode in .js files
(add-to-list 'auto-mode-alist '("\\.js\\'". web-mode))

;; Fetch path from shell and set as Emacs path
;; Used to make sure 'rg' and 'ag' are accessible to Emacs
(defun set-exec-path-from-shell-PATH ()
  "Set path for Emacs executed from command line."
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
