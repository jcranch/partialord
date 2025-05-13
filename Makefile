build:
	stack test

documents: README.html

%.html: %.md
	markdown $< >$@
