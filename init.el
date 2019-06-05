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

;;
;; =============================
;; ------     Plugins     ------
;; =============================
;;


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
  :after (evil lsp-mode)
  :ensure t
  :config
  (require 'lsp-mode)
  (evil-collection-init)
  (evil-collection-define-key 'normal 'go-mode-map "gd" 'lsp-find-definition))

;; Key binding explanation popup
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-max-display-columns 3)
  (setq which-key-add-column-padding 5)
  (which-key-mode))

;; Org Mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t)

;; Key bindings
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (setq-default evil-escape-key-sequence "ESC")

  (general-define-key
   :states 'normal
   :keymap 'override
    "+" '(enlarge-window-horizontally :wk "+")
    "-" '(shrink-window-horizontally :wk "-"))

  (general-create-definer my-leader-def
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC")

  ;; Prevent keymaps from being overridden
  (general-override-mode)

  ;; General bindings
  (my-leader-def
    "ESC" '(keyboard-quit :wk t)
    "SPC" '(execute-extended-command :wk "Command Search")
    "TAB" '(previous-buffer :wk "Previous Buffer"))

  ;; Help bindings
  (my-leader-def
    "h" '(:ignore t :wk "Help")
    "hm" '(describe-mode :wk "Mode"))

  ;; Buffer bindings
  (my-leader-def
    "b" '(:ignore t :wk "Buffer")
    "bk" '(kill-buffer :wk "Kill"))

  ;; Window bindings
  (my-leader-def
    "w" '(:ignore t :wk "Window")
    "wb" '(balance-windows :wk "Balance")
    "ww" '(toggle-truncate-lines :wk "Toggle Word Wrap"))

  ;; Markdown bindings
  (my-leader-def
    "m" '(:ignore t :wk "Markdown")
    "mf" '(markdown-toggle-markup-hiding :wk "Toggle Formatting")
    "ml" '(markdown-live-preview-mode :wk "Live Preview")
    "mi" '(markdown-toggle-inline-images :wk "Toggle Images"))

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
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.1))

;; Elisp formatter
(use-package elisp-format :ensure t)

;; Support for Markdown editing
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-header-scaling t
        markdown-hide-urls t
        markdown-enable-math t
        markdown-command "pandoc"
        markdown-display-remote-images t
        markdown-fontify-code-blocks-natively t)
  :config
  (setq markdown-enable-wiki-links t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-link-space-sub-char "-"
        markdown-asymmetric-header t
        markdown-nested-imenu-heading-index t
        markdown-max-image-size '(640 . 480)
        markdown-hr-strings
        '("------------------------------------------------------------------------------"
          "*** *** ***"
          "--- --- ---")))

;; Rust Language Support
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))
(use-package flycheck-rust :ensure t)
(use-package racer :ensure t)
(use-package cargo :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Go Language Support
(use-package go-mode :ensure t)

;; Theme install
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord t))

;; Evil escaping
(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (go-mode . lsp)
  :config
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t))

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; Sensible SQL indentation
(use-package sql-indent
  :ensure t
  :after sql
  :config
  (add-hook 'sql-mode-hook #'sqlind-minor-mode))

;;
;; =============================
;; ------  Configuration  ------
;; =============================
;;

(require 'evil)
(require 'magit)
(require 'evil-magit)
(require 'transient)

;; Gofmt on save
(add-to-list 'exec-path "/usr/local/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Set where the custom variables are stored
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Move backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Translate ESC to C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Open Magit on switch project and remove all other panes
(setq counsel-projectile-switch-project-action
      '(lambda (x)
	 (magit-status x)
	 (delete-other-windows)
	 (message "Opening %s" x)))

;; Fix Magit help popup quit with ESC
(define-key transient-map        (kbd "<escape>") 'transient-quit-one)
(define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
(define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Fix keymap for changing windows while holding CTRL
(define-key evil-window-map "\C-h" 'evil-window-left)
(define-key evil-window-map "\C-j" 'evil-window-down)
(define-key evil-window-map "\C-k" 'evil-window-up)
(define-key evil-window-map "\C-l" 'evil-window-right)

(define-key evil-window-map "\C-l" 'evil-window-right)

;; Traverse visual lines instead of logical
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Open maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable the bloody annoying bell
(setq ring-bell-function 'ignore)

;; Enable which key info
(counsel-mode)
(counsel-projectile-mode)
(global-display-line-numbers-mode)

;; Set wrap point
(setq fill-column 120)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(global-visual-line-mode)

;; Disable the toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable web mode in .js files
(add-to-list 'auto-mode-alist '("\\.js\\'". web-mode))

;; Prevent default to TABS
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default evil-shift-round nil)
(setq-default indent-tabs-mode nil)

;; Disable Flymake mode whenever flycheck starts up
(add-hook 'flycheck-mode-hook #'flymake-mode-off)

;; Font sizing
(set-face-attribute 'default nil :height 140)

;; Fetch path from shell and set as Emacs path
;; Used to make sure 'rg' and 'ag' are accessible to Emacs
(defun set-exec-path-from-shell-PATH ()
  "Set path for Emacs executed from command line."
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
