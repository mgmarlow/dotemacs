;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Created via https://github.com/mgmarlow/start-emacs

;;; Code:

(defun osx-p () (memq window-system '(mac ns)))

(defun my--configure-fonts ()
  "Configure your default font family and size."
  (let ((base (if (osx-p) "Hack 14" "Hack 11")))
    (custom-set-faces
     `(default ((t :font ,base)))
     `(fixed-pitch ((t :inherit (default))))
     `(default ((t :inherit (default)))))))

(add-hook 'emacs-startup-hook #'my--configure-fonts)

;;; Setting tweaks:

(setq frame-inhibit-implied-resize t)
(setq ring-bell-function #'ignore)
(setq dired-auto-revert-buffer t)
(setq tab-always-indent 'complete)
(setq eshell-scroll-to-bottom-on-input 'this)
(setq mouse-yank-at-point t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq load-prefer-newer t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; Packages:

(use-package repeat
  :config
  (repeat-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-trio-dark))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the
  ;; binding available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package
  ;; such that the mode gets enabled right away.  Note that this
  ;; forces loading the package.
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :hook ((rust-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :bind (("C-c ." . eglot-code-action-quickfix)))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

;; From "Extending Start Emacs"

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :ensure t
  :after evil
  :config
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.2)
  ;; Prevent "jj" from escaping any mode other than insert-mode.
  (setq evil-escape-inhibit-functions
        (list (lambda () (not (evil-insert-state-p)))))
  (evil-escape-mode))

;;; Custom lisp modules:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package my-package-refresh
  :config
  (setq my-package-automatic-refresh-threshold (* 7 24)))

(use-package my-copilot-tree-sitter)

;;; init.el ends here
