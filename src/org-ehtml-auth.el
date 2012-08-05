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
      (elnode-method httpcon
        (GET  (elnode-auth-login-sender httpcon
                "/login/" (or (cdr (assoc "to" (elnode-http-params httpcon)))
                              "/")))
        (POST (let* ((params (elnode-http-params httpcon))
                     (password (cdr (assoc "password" params)))
                     (username (cdr (assoc "username" params)))
                     (redirect (cdr (assoc "redirect" params))))
                (elnode-auth-http-login httpcon username password redirect))))
    (elnode-method httpcon
        (GET  (org-ehtml-file-handler httpcon))
        (POST (condition-case token
                  (progn
                    (elnode-auth-cookie-check-p httpcon)
                    (org-ehtml-edit-handler httpcon))
                (elnode-auth-token ;; Authenticate
                 (elnode-http-start httpcon 401)
                 (elnode-http-return httpcon)))))))

(elnode-auth-define-scheme 'org-ehtml
 :redirect (elnode-auth-make-login-wrapper 'org-ehtml-auth-handler))
