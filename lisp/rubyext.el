;;; rubyext.el --- Ruby extensions                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for Ruby modes.

;;; Code:

(require 'project)

(defun rubyext--project-compile-command (command)
  "Like `project-compile', but non-interactive.

Argument COMMAND is a string, like \"bin/rspec\"."
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (compile command)))

(defun rubyext--execute-rspec (path)
  "Run rspec on PATH."
  (rubyext--project-compile-command
   (concat "bin/rspec" " " path)))

(defun rubyext--ends-with? (str suffix)
  "Check if STR ends with SUFFIX."
  (and (>= (length str) (length suffix))
       (string= (substring str (- (length str) (length suffix))) suffix)))

(defun rubyext--recursive-search-predicate (filename)
  "Narrow `directory-files-recursively' for quicker searches.

Argument FILENAME is the currently visited filename from the directory search."
  (cond
   ((string-match-p "node_modules" filename) nil)
   ((string-match-p ".bundle" filename) nil)
   ((string-match-p "tmp" filename) nil)
   (t t)))

(defun rubyext--locate-spec-file-candidates ()
  "Locate the spec file corresponding to the current buffer-file-name."
  (let* ((maybe-spec-file-name (concat (file-name-base buffer-file-name) "_spec.rb"))
         (regexp (format "^%s$" (regexp-quote maybe-spec-file-name)))
         ;; TODO This could be smarter by looking for a pack:
         (dir (project-root (project-current))))
    (directory-files-recursively dir regexp nil #'rubyext--recursive-search-predicate)))

;;;###autoload
(defun rubyext-rspec-matching-file (&optional args)
  "Run rspec on a file matching the current `buffer-file-name'.

Errors if no files are found.  If there's more than one candidate,
prompt via `completing-read'."
  (interactive)
  (if (rubyext--ends-with? buffer-file-name "_spec.rb")
      (rubyext--execute-rspec (apply #'concat buffer-file-name args))
    (let ((candidates (rubyext--locate-spec-file-candidates)))
      (cond
       ((= 0 (length candidates))
        (error "No matching rspec files found."))
       ((= 1 (length candidates))
        (rubyext--execute-rspec (car candidates)))
       (t
        (rubyext--execute-rspec (completing-read "Pick a spec: " candidates nil t)))))))

;;;###autoload
(defun rubyext-rspec-current-line ()
  "Like `rubyext-rspec-matching-file', but at the current line."
  (interactive)
  (rubyext--execute-rspec
   (concat buffer-file-name ":" (number-to-string (line-number-at-pos)))))

;;;###autoload
(defun rubyext-rubocop-current-file ()
  "Run rubocop on the current file."
  (interactive)
  (rubyext--project-compile-command
   (concat "bin/rubocop" " " (buffer-file-name))))

(provide 'rubyext)
;;; rubyext.el ends here
