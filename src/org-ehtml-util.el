;;; org-ehtml-util.el --- utility functions

;; Copyright (C) 2012 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: org web-server javascript html

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'cl-lib)

(defmacro while-visiting-file (file &rest body)
  "Execute BODY in a temporary buffer visiting FILE."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,file)
     ,@body))
(def-edebug-spec while-visiting-file (form body))

(defun file-contents (path)
  (while-visiting-file path (buffer-string)))

(defun replace-region (start end to-string)
  "Replace the text between START and END with TO-STRING."
  (interactive "r")
  (delete-region start end)
  (save-excursion (goto-char start) (insert to-string)))

(defvar org-ehtml-base
  (file-name-directory (or load-file-name (buffer-file-name))))

(provide 'org-ehtml-util)
;;; org-ehtml-util.el ends here
