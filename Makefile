EMACS ?= emacs
INFO  := evil-god-toggle.info
TEXI  := evil-god-toggle.texi
ORG   := README.org

## Override on the command line or in the environment to point at your
## evil / god-mode install dirs:
##   make test EVIL_DIR=/path/to/evil GOD_MODE_DIR=/path/to/god-mode
EVIL_DIR     ?= $(HOME)/.emacs.default/elpaca/builds/evil
GOD_MODE_DIR ?= $(HOME)/.emacs.default/elpaca/builds/god-mode

.PHONY: doc info test clean

## Default target: build the shipped .texi from README.org.
## Elpaca (or package.el / MELPA) compiles .texi -> .info at install
## time, so the .texi is the artifact committed to the repo.
doc: $(TEXI)

$(TEXI): $(ORG)
	$(EMACS) -Q --batch \
	  --eval "(require 'ox-texinfo)" \
	  --eval "(find-file \"$(ORG)\")" \
	  --eval "(org-texinfo-export-to-texinfo)"

## Optional local preview: also run makeinfo to produce the .info.
info: $(INFO)

$(INFO): $(TEXI)
	makeinfo $(TEXI)

test:
	$(EMACS) -Q --batch \
	  -L . -L $(EVIL_DIR) -L $(GOD_MODE_DIR) \
	  -l evil-god-toggle-tests.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f $(INFO) $(TEXI) *.elc
