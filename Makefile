.PHONY: compile simulate

compile:compile/input
	Rscript compile/src/compile_stan.R

simulate:simulate/input simulate/hand
	Rscript simulate/src/make_simulations.R
