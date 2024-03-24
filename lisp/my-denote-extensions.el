;;; my-denote-extensions.el --- Denote extensions    -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(defun my-denote--weekly-template ()
  (concat "* Friday"
          "\n\n"
          "* Thursday"
          "\n\n"
          "* Wednesday"
          "\n\n"
          "* Tuesday"
          "\n\n"
          "* Monday"
          "\n\n"
          "* Notes"))

(add-to-list 'denote-templates `(weekly . ,(my-denote--weekly-template)))

(defun my-denote-weekly ()
  "Find or create a weekly journal entry."
  (interactive)
  (let* ((title (concat "week-" (format-time-string "%G-%U" (current-time))))
         (pattern (concat ".*--" title))
         (matches (denote-directory-files-matching-regexp pattern)))
    (if matches
        (find-file (car matches))
      (denote title '("journal" "weekly") 'org nil nil 'weekly))))

(provide 'my-denote-extensions)
;;; my-denote-extensions.el ends here
