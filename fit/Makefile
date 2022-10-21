.PHONY: fit

all: fit

fit: output/fit_stan.touch output/fit_R.touch

output/fit_stan.touch:input hand
	Rscript src/fit_stan.R
	touch output/fit_stan.touch

output/fit_R.touch: $(wildcard input/*.csv) hand
	Rscript src/fit_R.R
	touch output/fit_R.touch
