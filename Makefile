.PHONY: build

README.md: README.Rmd
	r -e 'rmarkdown::render("$<", output_file="$@")'

build: $(shell find ./R -name "*.R") $(shell find ./tests -name "*.R") NAMESPACE
	R CMD build . && R CMD check --as-cran bayefdr_*
