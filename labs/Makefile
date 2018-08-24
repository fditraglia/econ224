RMDFILES = $(wildcard lab[0-2][0-9].Rmd)
PREFIXES = $(basename $(RMDFILES))
LABS = $(addsuffix .pdf, $(PREFIXES))

all : $(LABS)

%.pdf : %.Rmd
	Rscript -e 'library(rmarkdown); rmarkdown::render("$*.Rmd", "pdf_document")'

