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

(defun rubyext-rspec-current-file ()
  "Run rspec on the current file."
  (interactive)
  (rubyext--project-compile-command
   (concat "bin/rspec" " " (buffer-file-name))))

(defun rubyext-rspec-current-line ()
  "Run rspec for the test specified at the current line."
  (interactive)
  (rubyext--project-compile-command
   (concat "bin/rspec"
           " "
           (buffer-file-name)
           ":"
           (number-to-string (line-number-at-pos)))))

(defun rubyext-rubocop-current-file ()
  "Run rubocop on the current file."
  (interactive)
  (rubyext--project-compile-command
   (concat "bin/rubocop" " " (buffer-file-name))))

(provide 'rubyext)
;;; rubyext.el ends here
