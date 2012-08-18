EMACS := emacs

# Set these environment variables so that they point to the
# development directories of Org-mode and elnode.
ORGMODE ?= ~/path/to/org-mode/
ELNODE  ?= ~/path/to/elnode/

BATCH_EMACS=$(EMACS) --batch --execute \
   '(mapc (lambda (dir) (add-to-list (quote load-path) dir)) \
     `("$(ELNODE)" \
       ,(expand-file-name "lisp" "$(ORGMODE)") \
       ,(expand-file-name "contrib/lisp" "$(ORGMODE)") \
       ,(expand-file-name "src" default-directory) \
       ,(expand-file-name "lisp" (expand-file-name "test" default-directory))))'

# Package variables
NAME=org-ehtml
VERSION=0.$(shell date +%Y%m%d)
DOC="Export Org-mode files as editable web pages"
REQ=((elnode \"0.9.9\") (org-mode \"20120814\"))
DEFPKG="(define-package \"$(NAME)\" \"$(VERSION)\" \n  \"$(DOC)\" \n  '$(REQ))"
PACKAGE=$(NAME)-$(VERSION)

.PHONY: all src example package clean

SRC=$(wildcard src/*.el)
TEST=$(wildcard test/lisp/*.el)

all: src

src: $(SRC) $(TEST)
	$(BATCH_EMACS) -f batch-byte-compile $^

example: test/lisp/example.el
	$(BATCH_EMACS) -l $^

package: $(SRC) src/org-ehtml-client.js src/org-ehtml-client.css
	mkdir $(PACKAGE); \
	cp $^ $(PACKAGE); \
	echo -e $(DEFPKG) > $(PACKAGE)/$(NAME)-pkg.el; \
	tar cf $(PACKAGE).tar $(PACKAGE); \
	rm -r $(PACKAGE)

clean:
	rm -f $(SRC:.el=.elc) $(TEST:.el=.elc) $(NAME)-*.tar
