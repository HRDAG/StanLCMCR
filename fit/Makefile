.PHONY: fit clean

all: fit

fit: output/fit_stan.touch output/fit_R.touch

output/fit_stan.touch:input hand/fit.yaml src/fit_stan.R
	time Rscript src/fit_stan.R
	touch output/fit_stan.touch

output/fit_R.touch: input hand/fit.yaml src/fit_R.R
	time Rscript src/fit_R.R
	touch output/fit_R.touch

clean:
	rm output/*
