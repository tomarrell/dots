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
    :prefix "SPC")

  (general-create-definer my-local-leader-def
    ;; :prefix my-local-leader
    :prefix "SPC m")

  ;; Prevent keymaps from being overridden
  (general-override-mode)

  ;; General bindings
  (my-leader-def 'normal
    "SPC" '(execute-extended-command :which-key "M-x")
  )

  ;; Git bindings
  (my-leader-def 'normal
    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "git status")
  )

  ;; File bindings
  (my-leader-def 'normal
    "f" '(:ignore t :which-key "File")
    "ff" '(counsel-find-file :which-key "file file")
  )
)

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
(require 'which-key)
(which-key-mode)
(evil-mode 1)
(counsel-mode 1)

;; Set where the custom variables are stored
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq fill-column 120)
(setq tab-width 2)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Disable the toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
