.PHONY: clean

all: output/figures.touch

output/figures.touch: input
	Rscript src/generate_figures.R
	touch output/figures.touch

clean:
	rm -r output
