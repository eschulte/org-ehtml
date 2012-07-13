;;; org-ehtml-server.el --- elnode server for editable Org-mode files

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
(require 'elnode)
(require 'org-ehtml-client)


;;; elnode setup and handlers
(defconst org-ehtml-server-urls
  '(("^$"      . org-ehtml-default-handler)
    ("^edit/$" . org-ehtml-edit-handler)))

(defun org-ehtml-server-dispatcher-handler (httpcon)
  (elnode-log-access "org-ehtml" httpcon)
  (elnode-dispatcher httpcon org-ehtml-server-urls))

(defun org-ehtml-default-handler (httpcon)
  (let ((path ()))
    (elnode-send-file httpcon
                      (file-contents (org-ehtml-client-export path)))))

(defun org-ehtml-edit-handler (httpcon)
  (let ((params (elnode-http-params httpcon)))
    (message "These are the http-params: \n %s" params)))


;;; Org-mode file manipulation

(provide 'org-ehtml-server)
;;; org-ehtml-server.el ends here
