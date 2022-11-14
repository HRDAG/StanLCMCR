.PHONY: compile clean

all: compile

compile:$(wildcard input/*)
	Rscript src/compile_stan.R

clean:
	rm -r output/*
