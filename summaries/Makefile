.PHONY: clean

all: output/summaries.touch

output/summaries.touch: input hand/summaries.yaml src/generate_summaries.R
	Rscript src/generate_summaries.R
	touch output/summaries.touch

clean:
	rm -r output/*
