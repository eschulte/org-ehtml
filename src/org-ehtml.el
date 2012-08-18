;;; org-ehtml --- Export Org-mode files as editable web pages

;; Copyright (C) 2012 Eric Schulte <eric.schulte@gmx.com>

;; Author: Eric Schulte <eric.schulte@gmx.com>
;; Keywords: org elnode javascript html

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
(let* ((default-directory (file-name-directory
                           (or load-file-name (buffer-file-name)))))
  (require 'org-ehtml-client (expand-file-name "org-ehtml-client.el"))
  (require 'org-ehtml-server (expand-file-name "org-ehtml-server.el")))

(provide 'org-ehtml)
;;; org-ehtml.el ends here
