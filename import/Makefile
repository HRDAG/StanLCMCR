.PHONY: clean

all: output/simulations.touch output/datasets.touch

output/simulations.touch:hand src/make_simulations.R
	mkdir -p output
	Rscript src/make_simulations.R
	touch output/simulations.touch

output/datasets.touch:hand src/process_data.R
	mkdir -p output
	Rscript src/process_data.R
	touch output/datasets.touch

clean:
	rm output/*
