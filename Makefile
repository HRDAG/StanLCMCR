.PHONY: compile simulate fit_stan fit_R fit summaries write scaffold

all: scaffold write

write:summaries
	cd write && make

summaries:fit
	cd summaries && make

fit:simulate
	cd fit && make

simulate:compile
	cd simulate && make

compile:
	cd compile && make

scaffold:
	mkdir -p compile/output simulate/output fit/input fit/output summaries/output write/input write/output
	-cd fit/input && ln -s ../../compile/output models
	-cd fit/input && ln -s ../../simulate/output data
	-cd summaries && ln -s ../fit/output input
	-cd write/input && ln -s ../../summaries/output summaries
	-cd write/input && ln -s ../../fit/output fit
