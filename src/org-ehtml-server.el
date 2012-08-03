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

(defvar org-ehtml-docroot "~/org-web"
  "Document root from which to serve Org-mode files.")

(defvar org-ehtml-before-save-hook nil
  "Hook run in a file buffer before saving web edits.
If any function in this hook returns nil then the save is aborted
and the edit is reverted.")

(defvar org-ehtml-after-save-hook nil
  "Hook run in a file buffer after saving web edits.")

(defun org-ehtml-server-dispatcher-handler (httpcon)
  (elnode-log-access "org-ehtml" httpcon)
  (elnode-method httpcon
    (GET  (org-ehtml-file-handler httpcon))
    (POST (org-ehtml-edit-handler httpcon))))

(defun org-ehtml-file-handler (httpcon)
  (elnode-docroot-for org-ehtml-docroot :with file :on httpcon :do
    (elnode-send-file httpcon (org-ehtml-client-export-file file))))

(defun org-ehtml-edit-handler (httpcon)
  (let* ((params (elnode-http-params httpcon))
         (path                  (cdr (assoc "path" params)))
         (beg (string-to-number (cdr (assoc "beg"  params))))
         (end (string-to-number (cdr (assoc "end"  params))))
         (org                   (cdr (assoc "org"  params))))
    (if (= ?/ (aref path 0))
        (org-ehtml-update-file (substring path 1) beg end org)
      (error "path does not begin with a '/'"))
    (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
    (elnode-http-return httpcon
      (org-export-string org 'html org-ehtml-docroot))))

(defun org-ehtml-update-file (path beg end new) ;; TODO: sub-folders
  (org-babel-with-temp-filebuffer (expand-file-name path org-ehtml-docroot)
    (let ((orig (buffer-string)))
      (replace-region beg end new)
      (if (run-hook-with-args-until-failure 'org-ehtml-before-save-hook)
          (save-buffer)
        (replace-region (point-min) (point-max) orig)
        (error "edit rejected by `org-ehtml-before-save-hook'.")))
    (run-hooks 'org-ehtml-after-save-hook)))

(provide 'org-ehtml-server)
;;; org-ehtml-server.el ends here
