library(pacman)
pacman::p_load(here, yaml, dplyr)

source(here("fit", "src", "lib_fit_R.R"))

# Configuration file for fits
fit_params <- read_yaml(here("fit", "hand", "fit.yaml"))
R_settings <- fit_params$R_settings
datasets <- fit_params$datasets

for (i in 1:length(datasets)) {
    dataset_name <- datasets[[i]]
    df <- read.csv(here("fit", "input", "data", paste(dataset_name, ".csv", sep="")))
    model_q025 <- R_settings$prior_q025
    model_q975s <- R_settings$prior_q975

    if (is.null(model_q025)) {
        model_q025 <- 1.01
        model_q975s <- c(5)
    }

    for (q975 in model_q975s) {
        run_name <- paste("R", q975, dataset_name, sep="_")

        rscript_args <- c(here("fit/src/fit_stan_cli.R"), "--model", "R", "--dataset", dataset_name, "--q975", q975)

        if (fit_params$use_slurm) {
            system2("srun", c("--ntasks=1", "--nodes=1", paste("--cpus-per-task=", fit_params$settings$chains, sep=""), "Rscript", rscript_args), wait=FALSE)
        } else {
            system2("Rscript", rscript_args)
        }
    }
}
