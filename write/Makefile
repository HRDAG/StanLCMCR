.PHONY: clean

all: output/figures.touch output/blogpost.html

output/figures.touch: input src/generate_figures.R
	Rscript src/generate_figures.R
	touch output/figures.touch

output/blogpost.html: input src/blogpost.Rmd
	Rscript -e "rmarkdown::render('src/blogpost.Rmd', output_dir='output')"

clean:
	rm -r output
