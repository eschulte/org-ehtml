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
(defvar org-ehtml-docroot "~/"
  "Document root from which to serve Org-mode files.")

(defvar org-ehtml-server-urls
  '(("^/$"     . org-ehtml-landing-page)
    ("^/"      . org-ehtml-file-handler)
    ("^edit/$" . org-ehtml-edit-handler)))

(defun org-ehtml-landing-page (httpcon)
  "Default ehtml landing page."
  (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
  (elnode-http-return httpcon
    (concat
     "<html><head><title>org-ehtml</title></head>"
     "<body><center>Welcome to <tt>org-ehtml</tt>.</center></body></html>")))

(defun org-ehtml-server-dispatcher-handler (httpcon)
  (elnode-log-access "org-ehtml" httpcon)
  (elnode-dispatcher httpcon org-ehtml-server-urls))

(defun org-ehtml-file-handler (httpcon)
  (elnode-docroot-for org-ehtml-docroot :with file :on httpcon :do
    (elnode-send-file httpcon (org-ehtml-client-export-file file))))

(defun org-ehtml-edit-handler (httpcon)
  (let ((params (elnode-http-params httpcon)))
    (message "These are the http-params: \n %s" params)))

;; To run execute the following
;;
;;   (elnode-start 'org-ehtml-server-dispatcher-handler)
;;
;; and then point your browser to http://localhost:8000
;;
;; to stop run the following
;;
;;  (elnode-stop 8000)


;;; Org-mode file manipulation

(provide 'org-ehtml-server)
;;; org-ehtml-server.el ends here
