EMACS := emacs

# Set these environment variables so that they point to the
# development directories of Org-mode and elnode.
ORGMODE ?= ~/path/to/org-mode/
ELNODE  ?= ~/path/to/elnode/

BATCH_EMACS=$(EMACS) --batch --execute '(mapc (lambda (dir) (add-to-list (quote load-path) dir)) \
      `("$(ELNODE)" \
        ,(expand-file-name "lisp" "$(ORGMODE)") \
        ,(expand-file-name "contrib/lisp" "$(ORGMODE)") \
        ,(expand-file-name "src" default-directory) \
        ,(expand-file-name "lisp" (expand-file-name "test" default-directory))))'

.PHONY: all src test clean example

SRC=$(wildcard src/*.el)
TEST=$(wildcard test/lisp/*.el)

all: src

src: $(SRC)
	$(BATCH_EMACS) -f batch-byte-compile $^

test: $(TEST)
	$(BATCH_EMACS) -f batch-byte-compile $^

example: test/lisp/example.el
	$(BATCH_EMACS) -l $^

clean:
	rm -f $(SRC:.el=.elc) $(TEST:.el=.elc)
