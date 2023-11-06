;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This Init File is built on Crafted Emacs v2+:
;; https://github.com/SystemCrafters/crafted-emacs.

;;; Code:

(require 'cl-lib)

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

(cl-defun crafted-init (&key modules packages)
  (dolist (m modules)
    (require (intern (format "crafted-%s-packages" m)) nil :noerror))
  (dolist (p packages)
    (add-to-list 'package-selected-packages p))
  (package-install-selected-packages :noconfirm)
  (dolist (m modules)
    (require (intern (format "crafted-%s-config" m)) nil :noerror)))

;;;; Packages and crafted emacs modules

(crafted-init
 :modules (append
           '(defaults completion ui ide org writing)
           (when (osxp) '(osx)))
 :packages '(ef-themes
             magit
             prettier-js
             aggressive-indent
             package-lint-flymake
             exec-path-from-shell
             flymake-clippy))

;;;; Custom

(customize-set-variable 'crafted-ui-display-line-numbers t)
(customize-set-variable 'denote-directory "~/denote/")

(setq inhibit-startup-screen t
      ring-bell-function #'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(ef-themes-select 'ef-autumn)

(customize-set-variable 'display-line-numbers-type 'relative)
(electric-pair-mode t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Support running multiple Flymake backends with Eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'flymake)

  (defun manually-activate-flymake ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (flymake-mode 1))

  (add-hook 'eglot-managed-mode-hook #'manually-activate-flymake nil t))

;; Rust
(require 'flymake-clippy)
(add-hook 'rust-ts-mode-hook #'flymake-clippy-setup-backend)

;; javascript/typescript
(require 'prettier-js) ;; prettier-js command is not autoloaded, only the major mode
(customize-set-variable 'js-indent-level 2)
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-mode-map (kbd "C-c C-f") #'prettier-js))

;; Denote/org
(require 'org-tempo)

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

(add-hook 'org-mode-hook (lambda () (electric-pair-local-mode -1)))
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'lisp-data-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)

(add-to-list 'auto-mode-alist '("\\.astro\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

(global-set-key (kbd "C-c r") #'query-replace)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c n j") #'journal-create-weekly)

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
