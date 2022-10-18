library(pacman)
pacman::p_load(here, LCMCR, yaml, tidyverse)

fit_R <- function(data, a_alpha=0.25, b_alpha=0.25, seed=1, buffer_size = 10000, thinning = 100, in_list_label="1", not_in_list_label="0", K=10, trace=FALSE, burnin=10000, samples=2000) {
    data.factor <- data.frame(lapply(data, factor))
    R_sampler <- lcmCR(data.factor, K=K, a_alpha=a_alpha, b_alpha=b_alpha, seed=seed, buffer_size = buffer_size, thinning = thinning, in_list_label=in_list_label, not_in_list_label=not_in_list_label)

    if (trace) {
        R_sampler$Activate_Tracing()
        R_sampler$Set_Trace(c(R_sampler$Get_Param_List()))
    }

    R_estimates <- lcmCR_PostSampl(R_sampler, burnin = burnin, samples = samples, thinning = thinning, output = FALSE)

    list(estimates=R_estimates, sampler=R_sampler)
}

# Configuration file for fits
fit_params <- read_yaml(here("fit", "hand", "fit.yaml"))
R_settings <- fit_params$R_settings
datasets <- fit_params$datasets

for (i in 1:length(datasets)) {
    dataset_name <- datasets[[i]]
    df <- read.csv(here("fit", "input", paste(dataset_name, ".csv", sep="")))

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
