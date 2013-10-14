EMACS := emacs

# Set these environment variables so that they point to the
# development directories of Org-mode and elnode.
ELPADIR ?= ~/.emacs.d/elpa/
ORGMODE ?= $(wildcard $(ELPADIR)/org-mode*)

BATCH_EMACS=$(EMACS) --batch --execute \
   '(mapc (lambda (dir) (add-to-list (quote load-path) dir)) \
     `(,@(mapcar (lambda (p) (expand-file-name p "$(ELPADIR)")) \
                 (directory-files "$(ELPADIR)")) \
       ,(expand-file-name "lisp" "$(ORGMODE)") \
       ,(expand-file-name "contrib/lisp" "$(ORGMODE)") \
       ,(expand-file-name "src" default-directory) \
       ,(expand-file-name "lisp" (expand-file-name "test" default-directory))))'

# Package variables
NAME=org-ehtml
VERSION=0.$(shell date +%Y%m%d)
DOC="Export Org-mode files as editable web pages"
REQ=((elnode \"20130416.1626\") (org-plus-contrib \"20131007\"))
DEFPKG="(define-package \"$(NAME)\" \"$(VERSION)\" \n  \"$(DOC)\" \n  '$(REQ))"
PACKAGE=$(NAME)-$(VERSION)

.PHONY: all src example package clean check test

# Filter auth until sync'd with newest version of elnode
FULL_SRC=$(wildcard src/*.el)
SRC=$(filter-out src/org-ehtml-auth.el,$(FULL_SRC))
TEST=$(wildcard test/lisp/*.el)

all: src

show-path:
	$(BATCH_EMACS) --eval "(mapc (lambda (p) (message \"%S\" p)) load-path)"

src: $(SRC) $(TEST)
	$(BATCH_EMACS) -f batch-byte-compile $^

example: test/lisp/example.el
	$(BATCH_EMACS) -l $^

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
