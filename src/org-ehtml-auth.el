;;; org-ehtml-auth.el --- authenticated server for editable Org-mode files

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
(require 'org-ehtml-server)

(defun org-ehtml-auth-handler (httpcon)
  (elnode-log-access "org-ehtml" httpcon)
  (if (string= (elnode-http-pathinfo httpcon) "/login/")
      (org-ehtml-auth-login httpcon)
    (elnode-method httpcon
      (GET  (org-ehtml-file-handler httpcon))
      (POST
       (elnode-with-auth httpcon 'org-ehtml
         (org-ehtml-edit-handler httpcon))))))

(elnode-auth-define-scheme 'org-ehtml
 :redirect (elnode-auth-make-login-wrapper 'org-ehtml-auth-handler))

(defun org-ehtml-auth-login (httpcon)
  (elnode-method httpcon
    (GET  (org-ehtml-request-login httpcon))
    (POST (org-ehtml-process-login httpcon))))

(defun org-ehtml-request-login (httpcon)
  (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
  (elnode-http-return httpcon "<html><body>Login:</body></html>"))

(defun org-ehtml-process-login (httpcon)
  (error "`org-ehtml-process-login' is not implemented"))
