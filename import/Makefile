.PHONY: clean

all: output/simulations.touch output/datasets.touch

output/simulations.touch:hand/simulations.yaml src/make_simulations.R
	mkdir -p output
	Rscript src/make_simulations.R
	touch output/simulations.touch

output/datasets.touch:hand/datasets.yaml src/process_data.R
	mkdir -p output
	Rscript src/process_data.R
	touch output/datasets.touch

clean:
	rm output/*
