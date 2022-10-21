all: output/simulations.touch

output/simulations.touch:hand
	mkdir -p output
	Rscript src/make_simulations.R
	touch output/simulations.touch
