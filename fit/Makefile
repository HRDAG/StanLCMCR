.PHONY: fit clean

all: fit

fit: output/fit_stan.touch output/fit_R.touch

output/fit_stan.touch:input hand
	time Rscript src/fit_stan.R
	touch output/fit_stan.touch

output/fit_R.touch: input hand
	time Rscript src/fit_R.R
	touch output/fit_R.touch

clean:
	rm output/*
