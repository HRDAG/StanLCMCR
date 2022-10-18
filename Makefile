.PHONY: compile simulate fit_stan fit_R fit summaries write

compile:compile/input
	mkdir -p compile/output
	Rscript compile/src/compile_stan.R
	mkdir -p fit/input
	cd fit/input && ln -sf ../../compile/output/* .

simulate:simulate/input simulate/hand
	mkdir -p simulate/output
	Rscript simulate/src/make_simulations.R
	mkdir -p fit/input
	cd fit/input && ln -sf ../../simulate/output/* .

fit_stan:fit/input fit/hand
	mkdir -p fit/output
	Rscript fit/src/fit_stan.R
	mkdir -p diagnostics/input
	cd diagnostics/input && ln -sf ../../fit/output/* .

fit_R: fit/input fit/hand
	mkdir -p fit/output
	Rscript fit/src/fit_R.R
	mkdir -p summaries/input
	cd summaries/input && ln -sf ../../fit/output/* .

summaries: summaries/input summaries/hand
	mkdir -p summaries/output
	Rscript summaries/src/generate_summaries.R
	mkdir -p write/input
	cd write/input/ && ln -sf ../../summaries/output/* .

fit: fit_R fit_stan

write: write/input
	mkdir -p write/output
	Rscript write/src/generate_figures.R
