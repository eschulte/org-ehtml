;;; org-ehtml --- Export Org-mode files as editable web pages

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

;;; Commentary:

;; View and edit Org-mode files through a web server.  Org-ehtml
;; defines a new export backend for Org-mode (ox-ehtml) which extends
;; the default HTML exporter with Javascript to allow for editing of
;; the web page.  Org-ehtml serves Org-mode files exported with
;; ox-ehtml using the Emacs Web Server allowing for Org-mode files to
;; be edited interactively through the web browser and for the edits
;; to be applied to the local Org-mode files on disk and optionally
;; committed to a backing version control repository.

;;; Code:
(let ((default-directory (file-name-directory
                          (or load-file-name (buffer-file-name)))))
  (require 'ox-ehtml (expand-file-name "ox-ehtml.el"))
  (require 'org-ehtml-server (expand-file-name "org-ehtml-server.el")))

(provide 'org-ehtml)
;;; org-ehtml.el ends here
