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

(defvar org-ehtml-docroot
  (expand-file-name "public_org" elnode-config-directory)
  "Document root from which to serve Org-mode files.")

(defvar org-ehtml-before-save-hook nil
  "Hook run in a file buffer before saving web edits.
If any function in this hook returns nil then the edit is aborted.")

(defvar org-ehtml-after-save-hook nil
  "Hook run in a file buffer after saving web edits.")

(defun org-ehtml-handler (httpcon)
  (elnode-log-access "org-ehtml" httpcon)
  (elnode-method httpcon
    (GET  (org-ehtml-file-handler httpcon))
    (POST (org-ehtml-edit-handler httpcon))))

(defun org-ehtml-file-handler (httpcon)
  (let ((elnode-docroot-for-no-404 t) (elnode-docroot-for-no-cache t))
    (elnode-docroot-for org-ehtml-docroot :with file :on httpcon :do
      (if (file-directory-p file)
          (let ((pt (elnode-http-pathinfo httpcon)))
            (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
            (elnode-http-return httpcon
              (elnode--webserver-index
               org-ehtml-docroot file pt "^[^\.].*[^~]$")))
        (elnode-send-file httpcon
          (if (member (file-name-extension file) '("org" "html"))
              (org-ehtml-client-cached file) file))))))

(defun org-ehtml-edit-handler (httpcon)
  (let* ((params (elnode-http-params httpcon))
         (path       (substring (cdr (assoc "path" params)) 1))
         (beg (string-to-number (cdr (assoc "beg"  params))))
         (end (string-to-number (cdr (assoc "end"  params))))
         (org                   (cdr (assoc "org"  params))))
    (org-babel-with-temp-filebuffer (expand-file-name path org-ehtml-docroot)
      (let ((orig (buffer-string)))
        (replace-region beg end org)
        (if (run-hook-with-args-until-failure 'org-ehtml-before-save-hook)
            (save-buffer)
          (replace-region (point-min) (point-max) orig)
          (elnode-send-500 httpcon "edit failed `org-ehtml-before-save-hook'")))
      (run-hooks 'org-ehtml-after-save-hook))
    (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
    (elnode-http-return httpcon
      (org-export-string org 'html org-ehtml-docroot))))

(provide 'org-ehtml-server)
;;; org-ehtml-server.el ends here
