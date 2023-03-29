.PHONY: clean

all: output/simulations.touch output/datasets.touch output/CO.touch

output/simulations.touch:hand/simulations.yaml src/make_simulations.R
	mkdir -p output
	Rscript src/make_simulations.R
	touch output/simulations.touch

output/datasets.touch:hand/datasets.yaml src/process_data.R
	mkdir -p output
	Rscript src/process_data.R
	touch output/datasets.touch

output/CO.touch:hand/CO.yaml src/make_CO_strata.R
	mkdir -p output
	Rscript src/make_CO_strata.R
	touch output/CO.touch


clean:
	rm output/*
