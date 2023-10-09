build:
	stack build

documents: README.html

%.html: %.md
	markdown $< >$@
