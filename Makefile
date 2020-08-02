SRCDIR := .emacs.d/lisp
SRCS := $(wildcard $(SRCDIR)/*.el)
TARGETS := $(SRCS:.el=.elc)

.PHONY: all clean
all: emacs

clean:
	rm -rf $(TARGETS)

emacs: $(TARGETS)

%.elc: %.el
	emacs -l .emacs.d/init.el -batch -f batch-byte-compile $^
