;;; my-copilot-tree-sitter.el --- Github copilot + tree-sitter compatibility  -*- lexical-binding: t; -*-

;;; Commmentary:

;; Add tree-sitter modes to copilot mode map.

;;; Code:

(defun my--copilot-tree-sitter-mode-map (recipe)
  "Return a copilot mode map for treesit-auto RECIPE.

Treesiter modes follow a *-ts-mode convention, so we need to map
*-ts to the respective copilot language name."
  (let ((mode-str (symbol-name (treesit-auto-recipe-ts-mode recipe))))
    (cons (substring mode-str 0 (- (length mode-str) (length "-mode")))
          (treesit-auto-recipe-lang recipe))))

(defun my-copilot-tree-sitter-init-alist ()
  (require 'treesit-auto)
  (dolist (recipe treesit-auto-recipe-list)
    (add-to-list 'copilot-major-mode-alist (my--copilot-tree-sitter-mode-map recipe))))

;; This package is not in MELPA, so install it via package-vc-install:
;; https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :after treesit-auto
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  :config
  (my-copilot-tree-sitter-init-alist))

(provide 'my-copilot-tree-sitter)
;;; my-copilot-tree-sitter.el ends here
