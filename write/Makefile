.PHONY: clean

all: output/figures.touch output/blogpost.html output/diagnostics/traceplots.pdf

output/figures.touch: input src/generate_figures.R
	Rscript src/generate_figures.R
	touch output/figures.touch

output/blogpost.html: input src/blogpost.Rmd
	Rscript -e "rmarkdown::render('src/blogpost.Rmd', output_dir='output')"

output/diagnostics/traceplots.pdf: input src/generate_traceplots.R
	Rscript src/generate_traceplots.R

clean:
	rm -r output && mkdir -p output/diagnostics
