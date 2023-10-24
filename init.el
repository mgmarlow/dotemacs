;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This Init File is built on Crafted Emacs v2+:
;; https://github.com/SystemCrafters/crafted-emacs.

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load (expand-file-name "modules/crafted-init-config" crafted-emacs-home))

(defun osxp () (eq system-type 'darwin))

(defun configure-fonts ()
  (let ((base (if (osxp) "Hack 14" "Hack 11")))
    (custom-set-faces
     `(default ((t :font ,base)))
     `(fixed-pitch ((t :inherit (default))))
     `(default ((t :inherit (default)))))))

(add-hook 'emacs-startup-hook #'configure-fonts)

;;;; Packages

(require 'org-tempo)
(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-ui-packages)
(require 'crafted-ide-packages)
(require 'crafted-org-packages)
(require 'crafted-writing-packages)
(add-to-list 'package-selected-packages 'ef-themes)
(add-to-list 'package-selected-packages 'evil-escape)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'prettier-js)

(package-install-selected-packages :noconfirm)

;;;; Configuration

(customize-set-variable 'crafted-ui-display-line-numbers t)
(customize-set-variable 'denote-directory "~/denote/")

(require 'crafted-defaults-config)
(require 'crafted-completion-config)
(require 'crafted-evil-config)
(require 'crafted-ui-config)
(require 'crafted-ide-config)
(require 'crafted-writing-config)
(when (osxp) (require 'crafted-osx-config))
(crafted-ide-configure-tree-sitter)

;;;; Custom

(setq inhibit-startup-screen t
      ring-bell-function #'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(ef-themes-select 'ef-autumn)

(customize-set-variable 'display-line-numbers-type 'relative)

(electric-pair-mode t)
(require 'prettier-js) ;; prettier-js command is not autoloaded, only the major mode

;; evil-escape
(setq evil-escape-key-sequence (kbd "jj")
      evil-escape-delay 0.2
      evil-escape-inhibit-functions (list (lambda () (not (evil-insert-state-p)))))
(evil-escape-mode)

(add-hook 'org-mode-hook (lambda () (electric-pair-local-mode -1)))
(add-hook 'lisp-data-mode-hook #'aggressive-indent-mode)

(add-to-list 'auto-mode-alist '("\\.astro\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

(global-set-key (kbd "C-c r") #'query-replace)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c n j") #'journal-create-weekly)

(defun journal-create-weekly ()
  "Find or create a weekly journal entry."
  (interactive)
  (require 'denote)
  (let* ((title (concat "week-" (format-time-string "%G-%U" (current-time))))
         (pattern (concat ".*--" title))
         (matches (denote-directory-files-matching-regexp pattern)))
    (if matches
        (find-file (car matches))
      (denote title '("journal" "weekly") 'org nil nil 'weekly))))

(defun kill-other-buffers ()
  "Kill all non-special buffers other than the current."
  (interactive)
  (let* ((normie-buffers (cl-remove-if-not #'buffer-file-name (buffer-list)))
         (bufs (delete (current-buffer) normie-buffers)))
    (when (yes-or-no-p
           (format "Kill %d buffers? " (length bufs)))
      (mapc #'kill-buffer bufs))))

(require 'server)
(unless (server-running-p) (server-start))
