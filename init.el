;; Personal Emacs Configuration

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

;; =============================
;; ------     Plugins     ------
;; =============================

(use-package evil :ensure t)
(use-package which-key :ensure t)
(use-package general :ensure t
             :config
             (general-evil-setup t)

             ;; * Prefix Keybindings
             ;; :prefix can be used to prevent redundant specification of prefix keys
             ;; again, variables are not necessary and likely not useful if you are only
             ;; using a definer created with `general-create-definer' for the prefixes
             ;; (defconst my-leader "SPC")
             ;; (defconst my-local-leader "SPC m")

             (general-create-definer my-leader-def
                                     ;; :prefix my-leader
                                     :states '(normal visual)
                                     :keymaps 'override
                                     :prefix "SPC")

             (general-create-definer my-local-leader-def
                                     ;; :prefix my-local-leader
                                     :prefix "SPC m")

             ;; Prevent keymaps from being overridden
             (general-override-mode)

             ;; General bindings
             (my-leader-def
               "SPC" '(execute-extended-command :which-key "M-x")
               "TAB" '(previous-buffer :which-key "TAB"))

             (my-leader-def
               "b" '(:ignore t :which-key "Buffer")
               "bk" '(kill-buffer :which-key "kill buffer"))

             ;; Git bindings
             (my-leader-def
               "g" '(:ignore t :which-key "Git")
               "gs" '(magit-status :which-key "git status"))

             ;; File bindings
             (my-leader-def
               "f" '(:ignore t :which-key "File")
               "ff" '(counsel-find-file :which-key "find file"))

						 ;; Ag bindings
						 (my-leader-def
							 "s" '(:ignore t :which-key "Search")
							 "sp" '(counsel-projectile-rg :which-key "Grep Project"))

             ;; Project bindings
             (my-leader-def
               "p" '(:ignore t :which-key "Project")
               "pp" '(counsel-projectile-switch-project :which-key "switch project")
               "pf" '(counsel-projectile-find-file :which-key "find file")
               "pd" '(counsel-projectile-find-dir :which-key "find dir")
               "pb" '(projectile-discover-projects-in-directory :which-key "discover projects")))

;; Allow up and down in results with Vim keybindings
(use-package ivy :ensure t
             :config
             (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
             (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
             (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-immediate-done)
             (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

;; Counsel installs Swiper as dependency
(use-package counsel :ensure t)

(use-package projectile :ensure t)
(use-package counsel-projectile :ensure t)

;; Magit, Git porcelain
(use-package magit :ensure t)
(use-package evil-magit :ensure t)
(use-package git-timemachine :ensure t)

;; JS Syntax Highlighting
(use-package web-mode :ensure t)

;; Theme install
(use-package solarized-theme :ensure t
             :config
             (require 'solarized)
             (deftheme solarized-light "The light variant of the Solarized colour theme")
             (create-solarized-theme 'light 'solarized-light)
             (provide-theme 'solarized-light)
             (load-theme 'solarized-light t))


;; =============================
;; ------  Configuration  ------
;; =============================
(setq evil-want-C-u-scroll t)
(setq custom-file "~/.emacs.d/custom.el")

;; Open Magit on switch project
(setq counsel-projectile-switch-project-action #'magit-status)

;; Set where the custom variables are stored
(load custom-file)

(require 'which-key)
(require 'evil)
(require 'magit)
(require 'evil-magit)

;; Fix Evil scrolling up keybindings
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
            (lambda ()
              (interactive)
              (evil-delete (point-at-bol) (point))))

(which-key-mode)
(evil-mode 1)
(counsel-mode 1)
(counsel-projectile-mode 1)

(setq fill-column 120)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Disable the toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Fetch path from shell and set as Emacs path
;; Used to make sure 'rg' and 'ag' are accessible to Emacs
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
	 This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; Enable web mode in .js files
(add-to-list 'auto-mode-alist '("\\.js\\'". web-mode))

;; Setup indentation
;; 2 spaces
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq evil-shift-width n)
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n)) ; css-mode

(defun my-personal-code-style ()
  (interactive)
  (message "My personal code style!")
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (my-setup-indent 2))

(defun my-setup-develop-environment ()
  (interactive)
  (my-personal-code-style))

;; prog-mode-hook requires emacs24+
(add-hook 'prog-mode-hook 'my-setup-develop-environment)
;; a few major-modes does NOT inherited from prog-mode
(add-hook 'web-mode-hook 'my-setup-develop-environment)
