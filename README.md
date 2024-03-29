# Stan implementations of Latent Class Multiple Capture-Recapture (LCMCR)

In this repository, we reimplement [LCMCR](https://cran.r-project.org/web/packages/LCMCR/index.html) in [Stan](https://mc-stan.org) (see [paper](https://onlinelibrary.wiley.com/doi/pdf/10.1111/biom.12502?casa_token=xzENWZqrOeIAAAAA:zgosPYNHnSRy23hm8fOtstkMBzYGagL50Vv_QZ2uKx-dPT_-NBPjRLcY-vvLIGKOJ0wjxFj_KXQA3-aR)), and diagnose and fix the causes of poor mixing and divergent transitions.

## Project structure

The repository structure is broken up into atomic tasks joined together using Make (see this blogpost on [Principled Data Processing](https://hrdag.org/2016/06/14/the-task-is-a-quantum-of-workflow/)). In particular we have the following directory structure:

- `stan`: the raw Stan source code for the models
- `compile`: compiles the Stan code
- `import`: generates the simulated (and other) data
- `fit`: fits the compiled models (and the R LCMCR code by comparison) against all the datasets
- `summaries`: creates summaries of each of the fits
- `write`: generates the figures ultimately used in the blogpost

## Installation and running

We are using an experimental version of the R LCMCR package that takes two parameters: `a_lambda` and `b_lambda`. Create a directory called `opt/` from the root project directory, and install the package there: `install.packages('R_Package_basic/output/LCMCR_0.5.0.tar.gz', lib = '~/path/to/project/opt')`.

Aside from the experimental LCMCR package, the main R dependency is `pacman`. If it is not installed, run

```
Rscript -e 'install.packages("pacman")'
```

in the terminal. To run the project from end-to-end, which will install the necessary dependencies, run `make`.

Note: `make` will install the required R dependencies, including `cmdstanr`, which will in turn install Stan.
