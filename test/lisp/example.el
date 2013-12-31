;;; batch.el --- Batch Emacs test for org-ehtml

;;; Instructions:
;;
;; 1. Set the ORGMODE environment variable to point to the development
;;    directory for Org-mode.
;;    
;; 2. Launch an Emacs process which will load this configuration and
;;    only this configuration using the Makefile as follows
;;
;;      make example
;; 
;; 3. You will now be serving the example test files on port 3333.
;;    Browse to http://localhost:3333/simple.org and try to perform an
;;    edit through the web interface.

;;; Code:
(require 'test-org-ehtml)

(defvar org-ehtml-port 3333 "Port used to run this example server.")

(setq
 debug-on-error t                 ; show debug info for any errors
 org-html-postamble nil           ; don't export a postamble
 org-ehtml-docroot test-org-ehtml-example-dir)

;; stop the default org-ehtml server
(ws-start org-ehtml-handler org-ehtml-port)

(message "Serving example Org-mode file to http://localhost:%s" org-ehtml-port)
(message "quit with C-c")
(while t (sit-for 1))
