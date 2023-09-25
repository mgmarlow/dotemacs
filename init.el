;;;; https://github.com/SystemCrafters/crafted-emacs

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load "~/projects/crafted-emacs/modules/crafted-init-config")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (custom-set-faces
	     `(default ((t :font "Hack 11")))
	     `(fixed-pitch ((t :inherit (default))))
	     `(default ((t :inherit (default)))))))

;;;; Packages

(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-ui-packages)
(require 'crafted-ide-packages)
(add-to-list 'package-selected-packages 'ef-themes)
(add-to-list 'package-selected-packages 'evil-escape)

(package-install-selected-packages :noconfirm)

;;;; Configuration

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ef-themes-select 'ef-autumn)
(require 'crafted-defaults-config)
(require 'crafted-completion-config)
(require 'crafted-evil-config)
(require 'crafted-ui-config)
(require 'crafted-ide-config)
(crafted-ide-configure-tree-sitter '(protobuf))
(setq evil-escape-key-sequence "jj"
      evil-escape-delay 0.2
      evil-escape-inhibit-functions (list
				     (lambda () (not (evil-insert-state-p)))))
(evil-escape-mode)
(electric-pair-mode t)

(add-to-list 'auto-mode-alist '("\\.astro\\'" . html-mode))
