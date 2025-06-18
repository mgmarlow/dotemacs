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
      require-final-newline t
      auto-save-default nil)
(setq-default indent-tabs-mode nil)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(use-package autorevert
  :config
  (global-auto-revert-mode))

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
  (let ((base "Hack 15"))
    (custom-set-faces
     `(default ((t :font ,base)))
     `(fixed-pitch ((t :inherit (default))))
     `(default ((t :inherit (default)))))))

(add-hook 'emacs-startup-hook #'my--configure-fonts)

(use-package modus-themes
  :ensure t
  :config
  (modus-themes-select 'modus-vivendi))

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

;; Exposing the handy `smerge-vc-next-conflict command'.
;; TODO: can this be autoloaded?
(use-package smerge-mode
  :ensure nil)

(use-package fennel-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Note: requires libenchant
(use-package jinx
  :ensure t
  :hook ((markdown-mode . jinx-mode)))

;; Note: requires ~/.authinfo
;; https://github.com/karthink/gptel?tab=readme-ov-file#optional-securing-api-keys-with-authinfo
;; e.g.
;; machine api.anthropic.com login apikey password sk-secret-anthropic-api-key-goes-here
(use-package gptel
  :ensure t
  :config
  (setq
   gptel-model "claude-3-opus-20240229"
   gptel-backend (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)))

(use-package multiple-cursors
  :ensure t)

(use-package format-all
  :ensure t)

;; TODO: mainline this into svelte-mode
;; Patch svelte-mode to support typescript-ts-mode
(with-eval-after-load 'svelte-mode
  (defun svelte--load-typescript-submode ()
    "Load `typescript-mode' and patch it."
    (when (require 'typescript-ts-mode nil t)
      (customize-set-variable 'typescript-ts-mode-indent-offset svelte-basic-offset)
      (defconst svelte--typescript-submode
        (svelte--construct-submode 'typescript-ts-mode
                                   :name "TypeScript"
                                   :end-tag "</script>"
                                   :syntax-table typescript-ts-mode--syntax-table
                                   :propertize #'typescript-ts--syntax-propertize
                                   :indent-function #'js-indent-line
                                   :keymap typescript-ts-mode-map)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; https://github.com/mgmarlow/helix-mode
(add-to-list 'load-path "~/projects/helix-mode/")
(use-package helix
  :after multiple-cursors
  :config
  (helix-mode)
  (helix-jj-setup 0.2)
  (helix-multiple-cursors-setup)
  (helix-define-typable-command
   "format"
   (lambda () (call-interactively #'format-all-buffer))))

;;; init.el ends here
