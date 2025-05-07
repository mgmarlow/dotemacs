;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Created via https://github.com/mgmarlow/start-emacs

;;; Code:

(defun mac? () (eq system-type 'darwin))

(setq frame-inhibit-implied-resize t
      ring-bell-function #'ignore
      dired-auto-revert-buffer t
      tab-always-indent 'complete
      eshell-scroll-to-bottom-on-input 'this
      mouse-yank-at-point t
      backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      load-prefer-newer t
      require-final-newline t)
(setq-default indent-tabs-mode nil)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(use-package repeat
  :config
  (repeat-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package savehist
  :init
  (savehist-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun my--configure-fonts ()
  (let ((base "Hack 14"))
    (custom-set-faces
     `(default ((t :font ,base)))
     `(fixed-pitch ((t :inherit (default))))
     `(default ((t :inherit (default)))))))

(add-hook 'emacs-startup-hook #'my--configure-fonts)

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package exec-path-from-shell
  :if (mac?)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package marginalia
  :ensure t
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
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

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :hook ((tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode))

(use-package zig-mode
  :ensure t)

(use-package fennel-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Note: requires libenchant
(use-package jinx
  :ensure t
  :hook ((markdown-mode . jinx-mode)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; https://github.com/mgmarlow/helix-mode
(add-to-list 'load-path "~/projects/helix-mode/")
(use-package helix
  :config
  (helix-mode))

;;; init.el ends here
