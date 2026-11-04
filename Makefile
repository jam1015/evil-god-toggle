EMACS ?= emacs
INFO  := evil-god-toggle.info
TEXI  := evil-god-toggle.texi
ORG   := README.org

.PHONY: doc info clean

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

clean:
	rm -f $(INFO) $(TEXI)
