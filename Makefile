EMACS := emacs

# Set these environment variables so that they point to the
# development directories of Org-mode and web-server.
WS ?= ~/.emacs.d/src/web-server
ORGMODE ?= ~/.emacs.d/src/org-mode

BATCH_EMACS=$(EMACS) --batch --execute \
   '(mapc (lambda (dir) (add-to-list (quote load-path) dir)) \
      `("$(shell pwd)" "$(WS)" \
       ,(expand-file-name "lisp" "$(ORGMODE)") \
       ,(expand-file-name "contrib/lisp" "$(ORGMODE)") \
       ,(expand-file-name "src" default-directory) \
       ,(expand-file-name "lisp" (expand-file-name "test" default-directory))))'

# Package variables
NAME=org-ehtml
VERSION=0.$(shell date +%Y%m%d)
DOC="Export Org-mode files as editable web pages"
REQ=((web-server \"20130416.826\") (org-plus-contrib \"20131007\"))
DEFPKG="(define-package \"$(NAME)\" \"$(VERSION)\" \n  \"$(DOC)\" \n  '$(REQ))"
PACKAGE=$(NAME)-$(VERSION)

.PHONY: all src example package clean check test

# Filter auth for now
SRC=$(wildcard src/*.el)
TEST=$(wildcard test/lisp/*.el)

all: src

show-path:
	$(BATCH_EMACS) --eval "(mapc (lambda (p) (message \"%S\" p)) load-path)"

src: $(SRC)
	$(BATCH_EMACS) -f batch-byte-compile $^

example: test/lisp/example.el
	$(filter-out --batch, $(BATCH_EMACS)) -Q -l $^

check: $(SRC) $(TEST)
	$(BATCH_EMACS) -l test/lisp/test-org-ehtml.el --eval '(ert t)'

test: check

%.txt: %
	$(BATCH_EMACS) $< -f org-ascii-export-to-ascii

%.html: %
	$(BATCH_EMACS) $< -f org-html-export-to-html

$(PACKAGE).tar: $(SRC) src/ox-ehtml.js src/ox-ehtml.css README.txt
	mkdir $(PACKAGE); \
	cp $^ $(PACKAGE); \
	mv $(PACKAGE)/README.txt $(PACKAGE)/README; \
	echo -e $(DEFPKG) > $(PACKAGE)/$(NAME)-pkg.el; \
	tar cf $(PACKAGE).tar $(PACKAGE); \
	rm -r $(PACKAGE)

package: $(PACKAGE).tar

clean:
	rm -f $(SRC:.el=.elc) $(TEST:.el=.elc) $(NAME)-*.tar
