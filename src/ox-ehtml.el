;;; ox-ehtml.el -- export of Org-mode to editable HTML

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
(require 'org)
(require 'ox-html)
(require 'org-ehtml-util)

(defvar ox-ehtml-style
  (concat
   "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n"
   (file-contents (expand-file-name "ox-ehtml.css" org-ehtml-base))
   "/*]]>*/-->\n</style>"))

(defvar ox-ehtml-jquery
  "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")

(defvar ox-ehtml-js
  (file-contents (expand-file-name "ox-ehtml.js" org-ehtml-base)))

(defun ox-ehtml-scripts ()
  (concat
   "<script type=\"text/javascript\" src=\""
   ox-ehtml-jquery "\"></script>"
   "<script type=\"text/javascript\">\n<!--/*--><![CDATA[/*><!--*/\n"
   ox-ehtml-js
   "/*]]>*///-->\n</script>\n"))

(defvar ox-ehtml-wrap-template
  (concat
   "<div class=\"edit_in_place\">%html-text</div>"
   "<div class=\"raw-org\" contents-begin=\"%begin\" contents-end=\"%end\">"
   "%org-text</div>"))

(defvar org-ehtml-everything-editable nil
  "Set to a true value to everything exported by org-ehtml editable.")

(defvar org-ehtml-editable-types
  '(paragraph plain-list table verbatim quote-block verse-block)
  "Types of elements whose children should not be editable.")

(defun ox-ehtml-editable-p (element info)
  (let ((parent (org-export-get-parent element)))
    (cond ((eq (car parent) 'headline)
           (or org-ehtml-everything-editable
               (member "EDITABLE" (org-export-get-tags parent info))))
          ((eq (car parent) 'org-data)
           (or org-ehtml-everything-editable
               (cl-some
                (lambda (keyword)
                  (let ((key (plist-get (cadr keyword) :key))
                        (val (plist-get (cadr keyword) :value)))
                    (and (string= "PROPERTY" key)
                         (string-match "editable \\(.+\\)" val)
                         (car (read-from-string (match-string 1 val))))))
                (cddr (cl-caddr parent)))))
          ((member (car parent) org-ehtml-editable-types) nil)
          (t (ox-ehtml-editable-p parent info)))))

(defmacro def-ehtml-wrap (html-function)
  "Defines and returns an ehtml-wrapped version of HTML-FUNCTION."
  (let ((fname (intern (concat "ox-ehtml"
                               (substring (symbol-name html-function) 10)))))
    `(defun ,fname (element contents info)
       ,(format "ehtml wrapper around `%s'." html-function)
       (let* ((original-contents (cl-copy-seq contents))
              (original-info     (cl-copy-seq info))
              (html-text (,html-function element contents info))
              (org-text  (or (org-element-interpret-data element)
                             original-contents
                             (error "no org-text found for %s" (car element)))))
         (if (ox-ehtml-editable-p element info)
             (org-fill-template ox-ehtml-wrap-template
              `(("html-text" . ,html-text)
                ("org-text"  . ,org-text)
                ("begin"     . ,(number-to-string
                                 (plist-get (cadr element) :begin)))
                ("end"       . ,(number-to-string
                                 (plist-get (cadr element) :end)))))
           html-text)))))

(org-export-define-derived-backend 'ehtml 'html
  :translate-alist
  `((paragraph   . ,(def-ehtml-wrap org-html-paragraph))
    (plain-list  . ,(def-ehtml-wrap org-html-plain-list))
    (table       . ,(def-ehtml-wrap org-html-table))
    (verbatim    . ,(def-ehtml-wrap org-html-verbatim))
    (quote-block . ,(def-ehtml-wrap org-html-quote-block))
    ;; (src-block   . ,(def-ehtml-wrap org-html-src-block))
    (verse-block . ,(def-ehtml-wrap org-html-verse-block))))

(defun ox-ehtml-export-to-html
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to an editable HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
	 (file (org-export-output-file-name extension subtreep pub-dir))
	 (org-export-coding-system org-html-coding-system)
         ;; custom headers
         (org-html-style-default (concat org-html-style-default "\n"
                                         ox-ehtml-style))
         (org-html-scripts (concat org-html-scripts "\n"
                                     (ox-ehtml-scripts))))
    (org-export-to-file 'ehtml file subtreep visible-only body-only ext-plist)))

(defun ox-ehtml-export-file (file)
  "Export FILE's contents to editable HTML."
  (save-window-excursion
    (find-file file)
    (ox-ehtml-export-to-html)))

(defun ox-ehtml-cached (file)
  "Export FILE to editable HTML if no previous export exists.
If a previous HTML export of FILE exists but is older than FILE
re-export."
  (cl-flet ((age (f)
                 (float-time
                  (time-subtract (current-time)
                                 (nth 5 (or (file-attributes (file-truename f))
                                            (file-attributes f)))))))
    (let* ((base (file-name-sans-extension file))
           (html (concat base ".html"))
           (org (concat base ".org")))
      (if (and (file-exists-p org)
               (or (not (file-exists-p html)) (> (age html) (age org))))
          (ox-ehtml-export-file org)
        html))))

(provide 'ox-ehtml)
;;; ox-ehtml.el ends here
