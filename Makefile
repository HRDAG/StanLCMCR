.PHONY: compile import fit_stan fit_R fit summaries write scaffold clean

all: scaffold write

write:summaries
	cd write && make

summaries:fit
	cd summaries && make

fit:import
	cd fit && make

import:compile
	cd import && make

compile:
	cd compile && make

clean:
	cd compile && make clean
	cd import && make clean
	cd fit && make clean
	cd summaries && make clean
	cd write && make clean

scaffold:
	mkdir -p compile/output import/output fit/input fit/output summaries/output write/input write/output
	-cd fit/input && ln -s ../../compile/output models
	-cd fit/input && ln -s ../../import/output data
	-cd summaries && ln -s ../fit/output input
	-cd write/input && ln -s ../../summaries/output summaries
	-cd write/input && ln -s ../../fit/output fit
