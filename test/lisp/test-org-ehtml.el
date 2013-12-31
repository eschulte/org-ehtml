;;; test-org-ehtml-js -- tests for org-ehtml

;; Copyright (C) 2012 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: org http javascript html

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
(require 'cl-lib)
(let ((src-dir (cl-reduce
                (lambda (dd file) (expand-file-name file dd))
                (reverse (list "org-ehtml.el" "src" ".." ".."))
                :initial-value (file-name-directory
                                (or load-file-name (buffer-file-name))))))
  (require 'org-ehtml src-dir))

(defvar test-org-ehtml-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defvar test-org-ehtml-example-dir
  (expand-file-name "examples" (expand-file-name ".." test-org-ehtml-dir)))

(defvar test-org-ehtml-simple-file
  (expand-file-name "simple.org" test-org-ehtml-example-dir))

(defvar test-org-ehtml-port 8888)

(defun test-org-ehtml-url-to-string (url &optional params)
  (async-shell-command
   (format "curl -m 4 %s localhost:%s/%s"
           (if params
               (format "-d %S"
                       (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p)))
                                  params "&"))
             "")
           test-org-ehtml-port url))
  (unwind-protect
      (with-current-buffer "*Async Shell Command*"
        (while (get-buffer-process (current-buffer)) (sit-for 0.1))
        (goto-char (point-min))
        (buffer-string))
    (kill-buffer "*Async Shell Command*")))

(defmacro test-org-ehtml-with (file html-var &rest body)
  (declare (indent 2))
  `(let* ((org-ehtml-docroot test-org-ehtml-example-dir)
          (srv (ws-start org-ehtml-handler test-org-ehtml-port)))
     (unwind-protect
         (let ((,html-var (test-org-ehtml-url-to-string
                           ,(car file) ,(cdr file))))
           ,@body)
       (ws-stop srv))))
(def-edebug-spec test-org-ehtml-with (form form body))


;;; Export tests
(ert-deftest org-ehtml-simple-export ()
  (cl-flet ((has (it)
              (goto-char (point-min))
              (should (re-search-forward (regexp-quote it) nil t))))
    (let ((html-file (expand-file-name
		      (file-name-nondirectory
		       (save-excursion
			 (find-file test-org-ehtml-simple-file)
			 (prog1 (org-ehtml-export-to-html)
			   (kill-buffer))))
		      test-org-ehtml-example-dir)))
      (while-visiting-file html-file
        ;; should include the ehtml css header
        (has ".edit_button")
        ;; should include the ehtml javascript header
        (has "set_clickable()")
        ;; the paragraph should be editable
        (has "<div class=\"edit_in_place\"><p")
        ;; the plain list should be editable
        (has "<div class=\"edit_in_place\"><ul")
        ;; the elements of the plain list should not be editable
        (has "<li>")))))

(ert-deftest org-ehtml-export-file ()
  (let ((html-file (expand-file-name
		    (file-name-nondirectory
		     (org-ehtml-export-file
		      test-org-ehtml-simple-file))
		    test-org-ehtml-example-dir)))
    (should (file-exists-p html-file))))


;;; server tests
(ert-deftest org-ehtml-serve-simple ()
  (test-org-ehtml-with ("simple.org") html
    (should (string-match "lorem" html))))

(ert-deftest org-ehtml-serve-complex ()
  (test-org-ehtml-with ("complex.org") html
    (should (string-match "lorem" html))))

(ert-deftest org-ehtml-serve-all-editable ()
  (test-org-ehtml-with ("all-editable.org") html
    (should (string-match "edit_in_place" html))))

(ert-deftest org-ehtml-post-request ()
  (let ((original (file-contents test-org-ehtml-simple-file))
        (params '(("path" . "/simple.org")
                  ("end"  . "577")
                  ("beg"  . "156")
                  ("org"  . "/foo/\n"))))
    (unwind-protect
        (test-org-ehtml-with ("simple.org" . params) html
          ;; ensure that the html export of "/foo/" is returned
          (should (string-match (regexp-quote ">foo</") html))
          ;; ensure that the file has been updated on disk
          (should (while-visiting-file test-org-ehtml-simple-file
                    (re-search-forward (regexp-quote "/foo/")))))
      (org-babel-with-temp-filebuffer test-org-ehtml-simple-file
        (delete-region (point-min) (point-max))
        (insert original)
        (save-buffer)))))

(ert-deftest org-ehtml-serve-difficult-chars ()
  (test-org-ehtml-with ("difficult-chars.org") html
    (should (string-match "difficult characters" html))))

(provide 'test-org-ehtml)
;;; test-org-ehtml.el ends here
