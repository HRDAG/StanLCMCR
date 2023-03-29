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

    results <- fit_R(df,
          a_alpha=R_settings$a_alpha,
          b_alpha=R_settings$b_alpha,
          seed=fit_params$seed,
          buffer_size=R_settings$buffer_size,
          thinning=R_settings$thinning,
          K=R_settings$K,
          trace=R_settings$trace,
          burnin=R_settings$burnin,
          samples=R_settings$n_iters)

    estimates <- results[["estimates"]]
    sampler <- results[["sampler"]]

    saveRDS(estimates, file=here("fit", "output", paste("R", dataset_name, "estimates.rds", sep="_")))
    saveRDS(sampler, file=here("fit", "output", paste("R", dataset_name, "model.rds", sep="_")))
}
