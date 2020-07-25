LATEXMK=latexmk -pdf
.PHONY: mk
mk:
	$(LATEXMK)

clean:
	latexmk -c
	rm -f *.{log,bbl,nav,rev,snm,vrb,vtc}

.PHONY: all clean
