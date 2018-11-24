.PHONY: all clean

ELCFILES = $(addsuffix .elc, $(basename $(wildcard *.el)))

all: $(ELCFILES)

%.elc : %.el
	@echo Compiling $<
	@emacs -batch -Q -f batch-byte-compile $<

clean:
	@rm -f *.elc
