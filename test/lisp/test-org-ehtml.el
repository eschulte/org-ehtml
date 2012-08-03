;;; test-org-ehtml-js -- tests for org-ehtml

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
(require 'ert)
(require 'org-test)
(require 'org-ehtml-client)
(require 'org-ehtml-server)

(defvar test-org-ehtml-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar test-org-ehtml-example-dir
  (expand-file-name "examples" (expand-file-name ".." test-org-ehtml-dir)))

(defvar test-org-ehtml-simple-file
  (expand-file-name "simple.org" test-org-ehtml-example-dir))

(defvar test-org-ehtml-port 8888)

(defun test-org-ehtml-url-to-string (url)
  (async-shell-command (format "curl localhost:%s/%s" test-org-ehtml-port url))
  (with-current-buffer "*Async Shell Command*"
    (while (get-buffer-process (current-buffer)) (sit-for 0.1))
    (goto-char (point-min))
    (buffer-string)))


;;; Export tests
(ert-deftest org-ehtml-simple-export ()
  (flet ((has (it)
              (goto-char (point-min))
              (should (re-search-forward (regexp-quote it) nil t))))
    (let ((html-file (org-test-with-temp-text-in-file
                         (file-contents test-org-ehtml-simple-file)
                       (org-ehtml-client-export-to-html))))
      (while-visiting-file html-file
        ;; should include the ehtml css header
        (has ".editable")
        ;; should include the ehtml javascript header
        (has "set_clickable()")
        ;; the paragraph should be editable
        (has "<div class=\"edit_in_place\"><p>")
        ;; the plain list should be editable
        (has "<div class=\"edit_in_place\"><ul>")
        ;; the elements of the plain list should not be editable
        (has "<li>")))))

(ert-deftest org-ehtml-export-file ()
  (let ((html-file (org-ehtml-client-export-file
                    test-org-ehtml-simple-file)))
    (should (file-exists-p html-file))))


;;; server tests
(ert-deftest org-ethml-elnode-server-file ()
  (let ((org-ehtml-docroot test-org-ehtml-example-dir))
    (unwind-protect
        (progn
          (elnode-start 'org-ehtml-server-dispatcher-handler
                        :port test-org-ehtml-port)
          (should (string-match "lorem"
                                (test-org-ehtml-url-to-string "simple.org")))
          (elnode-stop test-org-ehtml-port)))))

(provide 'test-org-ehtml)
;;; test-org-ehtml.el ends here
